import { Provider } from "../types/mod.ts";
import {
  Credential,
  Delegation,
  OutRef,
  ProtocolParameters,
  UTxO,
} from "../types/types.ts";
import { Assets } from "../types/mod.ts";
import { C } from "../core/mod.ts";
import { fromHex, getAddressDetails } from "../mod.ts";

type HydraCommand =
  | { tag: "GetUTxO" }
  | { tag: "NewTx"; transaction: string };

interface ServerResponse {
  tag: string;
  timestamp: Date;
  seq: number;
  headId: string;
}

interface GetUTxOResponse extends ServerResponse {
  tag: "GetUTxOResponse";
  utxo: Utxos;
}

interface TxValid extends ServerResponse {
  tag: "TxValid";
  transaction: Transaction;
}

interface TxInvalid extends ServerResponse {
  tag: "TxInvalid";
  transaction: Transaction;
  validationError: {
    reason: string;
  };
}

interface Transaction {
  id: string;
}

type Utxo = {
  address: string;
  datumhash?: string | undefined;
  inlineDatum?: string | undefined;
  referenceScript?: string | undefined;
  value: Assets;
};

type Utxos = {
  [key: string]: Utxo;
};

export class Hydra implements Provider {
  wsUrl: string;
  httpUrl: string;

  constructor(
    host: string,
    ssl: boolean = false,
  ) {
    this.wsUrl = `${ssl ? "wss" : "ws"}://${host}?history=no&snapshot-utxo=no`;
    this.httpUrl = `${ssl ? "https" : "http"}://${host}`;
  }

  async getProtocolParameters(): Promise<ProtocolParameters> {
    const result = await (
      await fetch(`${this.httpUrl}/protocol-parameters`)
    ).json();

    return {
      minFeeA: parseInt(result.txFeePerByte),
      minFeeB: parseInt(result.txFeeFixed),
      maxTxSize: parseInt(result.maxTxSize),
      maxValSize: parseInt(result.maxValueSize),
      keyDeposit: BigInt(result.stakeAddressDeposit),
      poolDeposit: BigInt(result.stakePoolDeposit),
      priceMem: parseFloat(result.executionUnitPrices.priceMemory),
      priceStep: parseFloat(result.executionUnitPrices.priceSteps),
      maxTxExMem: BigInt(result.maxTxExecutionUnits.memory),
      maxTxExSteps: BigInt(result.maxTxExecutionUnits.steps),
      coinsPerUtxoByte: BigInt(result.txFeePerByte),
      collateralPercentage: parseInt(result.collateralPercentage),
      maxCollateralInputs: parseInt(result.maxCollateralInputs),
      costModels: {
        "PlutusV1": result.costModels.PlutusV1 || {},
        "PlutusV2": result.costModels.PlutusV2 || {},
      },
    };
  }

  async getUtxos(addressOrCredential: string | Credential): Promise<UTxO[]> {
    return (await this.getSnapshotUtxos())
      .filter((utxo) => {
        if (typeof addressOrCredential === "string") {
          return addressOrCredential === utxo.address;
        } else {
          const { paymentCredential } = getAddressDetails(
            utxo.address,
          );
          paymentCredential?.hash;
          return paymentCredential?.hash === addressOrCredential.hash;
        }
      });
  }

  async getUtxosWithUnit(
    addressOrCredential: string | Credential,
    unit: string,
  ): Promise<UTxO[]> {
    const utxos = await this.getUtxos(addressOrCredential);
    return utxos.filter((utxo) => utxo.assets[unit] > 0n);
  }

  async getUtxoByUnit(unit: string): Promise<UTxO> {
    const utxos = (await this.getSnapshotUtxos())
      .filter((utxo) => utxo.assets[unit] > 0n);

    if (utxos.length > 1) {
      throw new Error("Unit needs to be an NFT or only held by one address.");
    }

    if (utxos.length < 1) {
      throw new Error("Unit not found at any address.");
    }

    return utxos[0];
  }

  async getUtxosByOutRef(outRefs: OutRef[]): Promise<UTxO[]> {
    const client = await this.hydraWsp({ tag: "GetUTxO" });
    const utxoResponse = await this.awaitMessage<GetUTxOResponse>(client);

    client.close();

    return outRefs.flatMap((outRef) => {
      const concatenatedRef = `${outRef.txHash}#${outRef.outputIndex}`;
      const maybeUtxo = utxoResponse.utxo[concatenatedRef];

      return maybeUtxo ? this.convertHydraUtxo(concatenatedRef, maybeUtxo) : [];
    });
  }

  getDelegation(rewardAddress: string): Promise<Delegation> {
    throw new Error("Delegation does not apply to Hydra.");
  }

  async getDatum(datumHash: string): Promise<string> {
    return (await this.getSnapshotUtxos())
      .filter((utxo) => utxo.datumHash === datumHash)[0].datum!;
  }

  async awaitTx(
    txHash: string,
    checkInterval?: number | undefined,
  ): Promise<boolean> {
    const client = new WebSocket(this.wsUrl);
    await new Promise((res) => {
      client.addEventListener("open", () => res(1), { once: true });
    });
    const isValid = await this.awaitTxValid(txHash, client, checkInterval);
    client.close();
    return isValid;
  }

  async submitTx(tx: string): Promise<string> {
    const client = await this.hydraWsp({
      tag: "NewTx",
      transaction: tx,
    });

    client.close();

    const coreTx = C.Transaction.from_bytes(fromHex(tx));
    const txHash = C.hash_transaction(coreTx.body()).to_hex();
    return txHash;
  }

  private async getSnapshotUtxos(): Promise<UTxO[]> {
    const client = await this.hydraWsp({ tag: "GetUTxO" });
    const utxoResponse = await this.awaitMessage<GetUTxOResponse>(client);

    client.close();

    return Object.entries(utxoResponse.utxo)
      .map(([outputRef, utxo]) => {
        return this.convertHydraUtxo(outputRef, utxo);
      });
  }

  private convertHydraUtxo(outputRef: string, utxo: Utxo): UTxO {
    const [txHash, outputIndex] = outputRef.split("#");

    return {
      txHash,
      outputIndex: Number(outputIndex),
      assets: utxo.value,
      address: utxo.address,
      datumHash: utxo.datumhash,
      datum: utxo.inlineDatum,
      scriptRef: utxo.referenceScript
        ? {
          type: "PlutusV2",
          script: utxo.referenceScript,
        }
        : undefined,
    };
  }

  private async awaitMessage<T>(client: WebSocket): Promise<T> {
    return await new Promise((res, rej) => {
      client.addEventListener("message", (msg: MessageEvent<string>) => {
        try {
          const serverResponse = JSON.parse(msg.data);
          if (serverResponse.tag == "CommandFailed") {
            rej(
              new Error(
                `Received "Command Failed" from Hydra. Is Hydra not in the right state?`,
              ),
            );
          } else {
            res(serverResponse as T);
          }
        } catch (e) {
          rej(e);
        }
      }, { once: true });
    });
  }

  /* Listen to all messages until receiving "CommandFailed", "TxValid", or "TxInvalid"
    The Caller is responsible for cleanup of `client`, even in the error case. */
  private async awaitTxValid(
    txHash: string,
    client: WebSocket,
    timeoutMs: number | undefined = 5000,
  ): Promise<boolean> {
    return await new Promise((res, rej) => {
      const listener = (msg: MessageEvent<string>) => {
        try {
          const serverResponse = JSON.parse(msg.data) as ServerResponse;
          if (serverResponse.tag == "CommandFailed") {
            rej(
              new Error(
                `Received "Command Failed" from Hydra. Is Hydra not in the right state?`,
              ),
            );
          } else if (serverResponse.tag == "TxValid") {
            if ((serverResponse as TxValid).transaction.id !== txHash) {
              return;
            }
            client.removeEventListener("message", listener);
            res(true);
          } else if (serverResponse.tag == "TxInvalid") {
            if ((serverResponse as TxInvalid).transaction.id !== txHash) {
              return;
            }
            client.removeEventListener("message", listener);
            rej(serverResponse);
          }
        } catch (e) {
          client.removeEventListener("message", listener);
          rej(e);
        }
      };

      client.addEventListener("message", listener);

      /* If the user calls awaitTxValid in an inappropriate way, it
         may leak the client and listener, or hang. This timeout guarantees cleanup. */

      setTimeout(() => {
        client.removeEventListener("message", listener);
        if (
          client.readyState !== WebSocket.CLOSING &&
          client.readyState !== WebSocket.CLOSED
        ) {
          rej(
            new Error(`Hydra never reported success or failure of ${txHash}.`),
          );
        }
      }, timeoutMs);
    });
  }

  private async hydraWsp(
    command: HydraCommand,
  ): Promise<WebSocket> {
    const client = new WebSocket(this.wsUrl);
    await new Promise((res) => {
      client.addEventListener("open", () => res(1), { once: true });
    });
    // The first message is always "Greetings"
    await new Promise((res) => {
      client.addEventListener("message", () => res(1), { once: true });
    });

    client.send(JSON.stringify(command));
    return client;
  }
}
