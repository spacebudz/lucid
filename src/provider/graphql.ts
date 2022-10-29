import { C, fromHex } from "../mod.ts";
import {
  Address,
  Assets,
  CostModels,
  Delegation,
  OutRef,
  ProtocolParameters,
  Provider,
  Slot,
  Transaction,
  TxHash,
  Unit,
  UTxO,
} from "../types/mod.ts";

export class GraphQL implements Provider {
  gqlUrl: string;
  submitUrl: string;
  authToken: string | undefined;
  constructor(gqlUrl: string, submitUrl: string, authToken?: string) {
    this.gqlUrl = gqlUrl;
    this.submitUrl = submitUrl;
    if (authToken) this.authToken = authToken;
  }

  async getProtocolParameters(): Promise<ProtocolParameters> {
    const ProtocolParametersQuery = `
    query getProtocolParameters {
      cardano {
        tip {
          slotNo
        }
        currentEpoch {
          protocolParams {
            minFeeA
            minFeeB
            poolDeposit
            keyDeposit
            coinsPerUtxoByte
            maxValSize
            maxTxSize
            priceMem
            priceStep
            maxTxExMem
            maxTxExSteps
            collateralPercent
            maxCollateralInputs
            costModels
          }
        }
      }
    }`;
    const fullGraphqlQuery = {
      "operationName": "getProtocolParameters",
      "query": ProtocolParametersQuery,
      "variables": {},
    };
    const qdata = await this.queryGraphQL(fullGraphqlQuery);
    const params: ProtocolParamsGQL =
      qdata.data.cardano.currentEpoch.protocolParams;
    return {
      minFeeA: parseInt(params.minFeeA.toString()),
      minFeeB: parseInt(params.minFeeB.toString()),
      maxTxSize: parseInt(params.maxTxSize.toString()),
      maxValSize: parseInt(params.maxValSize.toString()),
      keyDeposit: BigInt(params.keyDeposit),
      poolDeposit: BigInt(params.poolDeposit),
      priceMem: parseFloat(params.priceMem.toString()),
      priceStep: parseFloat(params.priceStep.toString()),
      coinsPerUtxoByte: BigInt(params.coinsPerUtxoByte),
      maxTxExMem: BigInt(params.maxTxExMem),
      maxTxExSteps: BigInt(params.maxTxExSteps),
      collateralPercentage: parseInt(params.collateralPercent.toString()),
      maxCollateralInputs: parseInt(params.maxCollateralInputs.toString()),
      costModels: params.costModels,
    };
  }

  async getCurrentSlot(): Promise<Slot> {
    const TipQuery = `
  query getCurrentTip {
      cardano {
        tip {
          slotNo
        }
      }
    }`;
    const fullGraphqlQuery = {
      "operationName": "getCurrentTip",
      "query": TipQuery,
      "variables": {},
    };

    const qdata = await this.queryGraphQL(fullGraphqlQuery);

    const slotNo = qdata.data.cardano.tip.slotNo;
    if (!slotNo) throw qdata.error;

    return slotNo;
  }

  async getUtxos(address: string): Promise<UTxO[]> {
    const UTxOsQuery = `
    query UTxOsByAddress($address: String!) {
      utxos(where: { address: { _eq: $address } }) {
        ${UTxOFields}
      }
    }`;
    const fullGraphqlQuery = {
      "operationName": "UTxOsByAddress",
      "query": UTxOsQuery,
      "variables": { address: address },
    };
    const qdata = await this.queryGraphQL(fullGraphqlQuery);

    const utxos: UtxosGraphql = qdata.data?.utxos;

    return utxos.map((r) => ({
      txHash: r.txHash,
      outputIndex: r.index,
      assets: (() => {
        const a: Assets = {};
        r.tokens.forEach((token: {
          asset: {
            policyId: string;
            assetName: string;
          };
          quantity: string;
        }) => {
          a[token.asset.policyId + token.asset.assetName] = BigInt(
            token.quantity,
          );
        });
        a["lovelace"] = BigInt(r.value);
        return a;
      })(),
      address,
      datumHash: r.datum?.hash,
      datum: r.datum?.bytes,
    }));
  }

  async getUtxosWithUnit(address: Address, unit: Unit): Promise<UTxO[]> {
    const AssetUTxOQuery = `
  query UTxOWithAssetQuery($address: String!, $policyId: String!, $asset: String!) {
    utxos(where: { 
      address: { _eq: $address }, _and: {
      tokens: { 
        asset: {
            policyId: {	
                _eq: $policyId
              }, 
            _and:	{
              assetName: {
                _eq: $asset
              }
            }
          }
        }
      }
    }) {
      ${UTxOFields}
    }
  }`;

    const fullGraphqlQuery = {
      "operationName": "UTxOWithAssetQuery",
      "query": AssetUTxOQuery,
      "variables": {
        address: address,
        policyId: unit.slice(0, 56),
        asset: unit.slice(56),
      },
    };
    const asstq = await this.queryGraphQL(fullGraphqlQuery);

    const utxos: UtxosGraphql = asstq.data?.utxos;
    return utxos.map((r) => ({
      txHash: r.txHash,
      outputIndex: r.index,
      assets: (() => {
        const a: Assets = {};
        r.tokens.forEach((token: {
          asset: {
            policyId: string;
            assetName: string;
          };
          quantity: string;
        }) => {
          a[token.asset.policyId + token.asset.assetName] = BigInt(
            token.quantity,
          );
        });
        a["lovelace"] = BigInt(r.value);
        return a;
      })(),
      address,
      datumHash: r.datum?.hash,
      datum: r.datum?.bytes,
    }));
  }

  async awaitTx(txHash: TxHash): Promise<boolean> {
    const TxQuery = `
      query TxQuery($txhash: Hash32Hex!) {
        transactions(where: { hash: { _eq: $txhash } }) {
          hash
        }
      }`;
    return await new Promise((res, _) => {
      const confirmation = setInterval(async () => {
        const fullGraphqlQuery = {
          "operationName": "TxQuery",
          "query": TxQuery,
          "variables": { txhash: txHash },
        };
        const txQ = await this.queryGraphQL(fullGraphqlQuery);

        if (
          !txQ.error && !txQ.errors && !txQ.data.transactions &&
          txQ.data.transactions.length > 0
        ) {
          clearInterval(confirmation);
          res(true);
          return;
        }
      }, 3000);
    });
  }

  async submitTx(tx: Transaction): Promise<TxHash> {
    const transaction = C.Transaction.from_bytes(fromHex(tx));
    const txhash = C.hash_transaction(transaction.body()).to_hex();
    const res = await fetch(this.submitUrl, {
      method: "POST",
      headers: { "Content-Type": "application/cbor" },
      body: transaction.to_bytes(),
    });
    if (res.status === 200) {
      return txhash;
    } else throw res;
  }

  async getUtxosByOutRef(outRefs: OutRef[]): Promise<UTxO[]> {
    const q = `query getUtxosByOutRef($outRef: [Hash32Hex]) {
      utxos(where: {
        transaction: {
          hash: {
            _in: $outRef
          }
        }
      }) {
        ${UTxOFields}
      }
    }`;

    const queryHashes = [...new Set(outRefs.map((outRef) => outRef.txHash))];
    const fullGraphqlQuery = {
      "operationName": "getUtxosByOutRef",
      "query": q,
      "variables": { outRef: queryHashes },
    };
    const utxos: UtxosGraphql = await this.queryGraphQL(fullGraphqlQuery);

    return graphqlSchemaUtxosToUtxos(
      utxos.reduce((acc: UtxosGraphql, utxos) => acc.concat(utxos), []).filter((
        utxo,
      ) =>
        outRefs.some((outRef) =>
          utxo.txHash === outRef.txHash && utxo.index === outRef.outputIndex
        )
      ),
    );
  }
  async getDelegation(rewardAddress: string): Promise<Delegation> {
    const q = `
    query getDelegation($address: String!){
      rewards(where: { address: {_eq: $address}}) {
        amount
        stakePool {
          id
        }
      }
    }`;
    const fullGraphqlQuery = {
      "operationName": "getDelegation",
      "query": q,
      "variables": { address: rewardAddress },
    };
    const dQ = await this.queryGraphQL(fullGraphqlQuery);
    if (dQ.data && dQ.data.length > 0) {
      return { rewards: dQ.data[0].amount, poolId: dQ.data[0].stakePool.id };
    } else if (dQ.error) throw dQ.error;
    return { rewards: 0n, poolId: null };
  }

  async getDatum(datumHash: string) {
    //currently it's not possible to filter out records where datum doesn't have bytes
    //TODO: watch out for future releases of Cardano graphql
    const q = `query getDatumFromHash($datumHash: Hash32Hex!) {
      utxos(where: { 
        datum: {hash : { _eq: $datumHash}}
      }) {
          datum {
            bytes
          }
      }
    }
    `;
    const fullGraphqlQuery = {
      "operationName": "getDatumFromHash",
      "query": q,
      "variables": { datumHash: datumHash },
    };
    const dQ = await this.queryGraphQL(fullGraphqlQuery);
    if (dQ.data) {
      for (const r of dQ.data) {
        if (r.datum.bytes) return r.datum.bytes;
      }
    } else if (dQ.error) throw dQ.error;
    return null;
  }

  async queryGraphQL(fullGraphqlQuery: {
    operationName: string;
    query: string;
    // deno-lint-ignore no-explicit-any
    variables: any;
    // deno-lint-ignore no-explicit-any
  }): Promise<any> {
    const headers: {
      "content-type": string;
      "Authorization"?: string;
    } = {
      "content-type": "application/json",
    };

    if (this.authToken) headers.Authorization = `Bearer ${this.authToken}`;

    const options = {
      "method": "POST",
      "headers": headers,
      "body": JSON.stringify(fullGraphqlQuery),
    };
    const response = await fetch(this.gqlUrl, options);
    return await response.json();
  }
}

const UTxOFields = `
  txHash
  index
  value
  datum {
    hash
    bytes
  }
  tokens {
    asset {
      policyId
      assetName
    }
    quantity
  }
  transactionOutput {
    address
  }
`

type ProtocolParamsGQL = {
  minFeeA: number;
  minFeeB: number;
  poolDeposit: number;
  keyDeposit: number;
  coinsPerUtxoByte: number;
  maxValSize: string;
  maxTxSize: number;
  priceMem: number;
  priceStep: number;
  maxTxExMem: string;
  maxTxExSteps: string;
  collateralPercent: number;
  maxCollateralInputs: number;
  costModels: CostModels;
};

type UtxosGraphql = {
  txHash: string;
  index: number;
  value: string;
  datum: {
    hash: string;
    bytes: string
  }
  tokens: {
    asset: {
      policyId: string;
      assetName: string;
    };
    quantity: string;
  }[];
  transactionOutput: {
    address: string;
  };
}[];

function graphqlSchemaUtxosToUtxos(utxos: UtxosGraphql): UTxO[] {
  return utxos.map((r) => ({
    txHash: r.txHash,
    outputIndex: r.index,
    assets: (() => {
      const a: Assets = {};
      r.tokens.forEach((token: {
        asset: {
          policyId: string;
          assetName: string;
        };
        quantity: string;
      }) => {
        a[token.asset.policyId + token.asset.assetName] = BigInt(
          token.quantity,
        );
      });
      a["lovelace"] = BigInt(r.value);
      return a;
    })(),
    address: r.transactionOutput.address,
    datumHash: r.datum?.hash,
    datum: r.datum?.bytes,
  }));
}
