import { OutRef } from "../core/types.ts";
import {
  ActiveDelegation,
  Assets,
  Credential,
  EmulatorState,
  Hasher,
  Network,
  OutputData,
  paymentCredentialOf,
  Provider,
  RelevantProtocolParameters,
  Utxo,
} from "../mod.ts";

export class Emulator implements Provider {
  network?: Network;
  private state: EmulatorState;

  constructor(
    accounts: {
      address: string;
      assets: Assets;
      outputData?: OutputData;
    }[],
  ) {
    const time = Date.now();
    this.network = { Emulator: time };

    const GENESIS_HASH = "00".repeat(32);
    const utxos = accounts.map((
      { address, assets, outputData },
      index,
    ) => {
      const { scriptRef, ...datumVariant } = outputData || {};

      const datumHash = "AsHash" in datumVariant
        ? Hasher.hashData(datumVariant.AsHash)
        : "Hash" in datumVariant
        ? datumVariant.Hash
        : undefined;

      const datum = "AsHash" in datumVariant
        ? datumVariant.AsHash
        : "Inline" in datumVariant
        ? datumVariant.Inline
        : undefined;

      return {
        txHash: GENESIS_HASH,
        outputIndex: index,
        address,
        assets,
        datumHash,
        datum,
        scriptRef,
      } as Utxo;
    });

    this.state = new EmulatorState(time, utxos);
  }

  getDatum(datumHash: string): Promise<string> {
    const datum = this.state.getDatum(datumHash);
    if (!datum) throw new Error(`No datum found: datum hash ${datumHash}`);
    return Promise.resolve(datum);
  }

  getProtocolParameters(): Promise<RelevantProtocolParameters> {
    return Promise.resolve(PROTOCOL_PARAMETERS_DEFAULT);
  }

  getDelegation(rewardAddress: string): Promise<ActiveDelegation> {
    const staking = this.state.getStaking(rewardAddress);
    return Promise.resolve({
      poolId: staking?.poolId || null,
      rewards: BigInt(staking?.rewards || 0),
    });
  }

  getUtxoByUnit(unit: string): Promise<Utxo> {
    const ledger = this.state.getLedger();

    const utxos = ledger.filter((utxo) => utxo.assets[unit] > 0n);

    if (utxos.length > 1) {
      throw new Error("Unit needs to be an NFT or only held by one address");
    }

    return Promise.resolve(utxos[0]);
  }

  getUtxos(addressOrCredential: string | Credential): Promise<Utxo[]> {
    const ledger = this.state.getLedger();

    const matchesAddress = (utxo: Utxo) =>
      typeof addressOrCredential === "string"
        ? utxo.address === addressOrCredential
        : addressOrCredential.hash === paymentCredentialOf(utxo.address).hash;

    return Promise.resolve(
      ledger.filter((utxo) => matchesAddress(utxo)),
    );
  }

  getUtxosByOutRef(outRefs: Array<OutRef>): Promise<Utxo[]> {
    const ledger = this.state.getLedger();

    return Promise.resolve(
      ledger.filter((utxo) =>
        outRefs.some((outRef) =>
          utxo.txHash === outRef.txHash &&
          utxo.outputIndex === outRef.outputIndex
        )
      ),
    );
  }

  getUtxosWithUnit(
    addressOrCredential: string | Credential,
    unit: string,
  ): Promise<Utxo[]> {
    const ledger = this.state.getLedger();

    const matchesAddress = (utxo: Utxo) =>
      typeof addressOrCredential === "string"
        ? utxo.address === addressOrCredential
        : addressOrCredential.hash === paymentCredentialOf(utxo.address).hash;

    return Promise.resolve(
      ledger.filter((utxo) => matchesAddress(utxo) && utxo.assets[unit] > 0n),
    );
  }

  awaitTx(txHash: string): Promise<boolean> {
    const mempool = this.state.getMempool();
    if (mempool.find((utxo) => utxo.txHash === txHash)) {
      this.state.awaitBlock();
      return Promise.resolve(true);
    }
    return Promise.resolve(true);
  }

  submit(tx: string): Promise<string> {
    return Promise.resolve(this.state.validate(tx));
  }

  distributeRewards(rewards: bigint) {
    this.state.distributeRewards(rewards);
  }

  awaitBlock(height = 1) {
    this.state.awaitBlock(height);
  }

  awaitSlot(slot = 1) {
    this.state.awaitSlot(slot);
  }

  now(): number {
    return this.state.getTime();
  }
}

export const PROTOCOL_PARAMETERS_DEFAULT: RelevantProtocolParameters = {
  minFeeA: 44,
  minFeeB: 155381,
  maxTxSize: 16384,
  maxValSize: 5000,
  keyDeposit: 2000000,
  poolDeposit: 500000000,
  priceMem: 0.0577,
  priceStep: 0.0000721,
  maxTxExMem: 14000000,
  maxTxExSteps: 10000000000,
  coinsPerUtxoByte: 4310,
  collateralPercentage: 150,
  maxCollateralInputs: 3,
  minfeeRefscriptCostPerByte: 15,
  costModels: {
    PlutusV1: [
      205665,
      812,
      1,
      1,
      1000,
      571,
      0,
      1,
      1000,
      24177,
      4,
      1,
      1000,
      32,
      117366,
      10475,
      4,
      23000,
      100,
      23000,
      100,
      23000,
      100,
      23000,
      100,
      23000,
      100,
      23000,
      100,
      100,
      100,
      23000,
      100,
      19537,
      32,
      175354,
      32,
      46417,
      4,
      221973,
      511,
      0,
      1,
      89141,
      32,
      497525,
      14068,
      4,
      2,
      196500,
      453240,
      220,
      0,
      1,
      1,
      1000,
      28662,
      4,
      2,
      245000,
      216773,
      62,
      1,
      1060367,
      12586,
      1,
      208512,
      421,
      1,
      187000,
      1000,
      52998,
      1,
      80436,
      32,
      43249,
      32,
      1000,
      32,
      80556,
      1,
      57667,
      4,
      1000,
      10,
      197145,
      156,
      1,
      197145,
      156,
      1,
      204924,
      473,
      1,
      208896,
      511,
      1,
      52467,
      32,
      64832,
      32,
      65493,
      32,
      22558,
      32,
      16563,
      32,
      76511,
      32,
      196500,
      453240,
      220,
      0,
      1,
      1,
      69522,
      11687,
      0,
      1,
      60091,
      32,
      196500,
      453240,
      220,
      0,
      1,
      1,
      196500,
      453240,
      220,
      0,
      1,
      1,
      806990,
      30482,
      4,
      1927926,
      82523,
      4,
      265318,
      0,
      4,
      0,
      85931,
      32,
      205665,
      812,
      1,
      1,
      41182,
      32,
      212342,
      32,
      31220,
      32,
      32696,
      32,
      43357,
      32,
      32247,
      32,
      38314,
      32,
      9462713,
      1021,
      10,
    ],
    PlutusV2: [
      205665,
      812,
      1,
      1,
      1000,
      571,
      0,
      1,
      1000,
      24177,
      4,
      1,
      1000,
      32,
      117366,
      10475,
      4,
      23000,
      100,
      23000,
      100,
      23000,
      100,
      23000,
      100,
      23000,
      100,
      23000,
      100,
      100,
      100,
      23000,
      100,
      19537,
      32,
      175354,
      32,
      46417,
      4,
      221973,
      511,
      0,
      1,
      89141,
      32,
      497525,
      14068,
      4,
      2,
      196500,
      453240,
      220,
      0,
      1,
      1,
      1000,
      28662,
      4,
      2,
      245000,
      216773,
      62,
      1,
      1060367,
      12586,
      1,
      208512,
      421,
      1,
      187000,
      1000,
      52998,
      1,
      80436,
      32,
      43249,
      32,
      1000,
      32,
      80556,
      1,
      57667,
      4,
      1000,
      10,
      197145,
      156,
      1,
      197145,
      156,
      1,
      204924,
      473,
      1,
      208896,
      511,
      1,
      52467,
      32,
      64832,
      32,
      65493,
      32,
      22558,
      32,
      16563,
      32,
      76511,
      32,
      196500,
      453240,
      220,
      0,
      1,
      1,
      69522,
      11687,
      0,
      1,
      60091,
      32,
      196500,
      453240,
      220,
      0,
      1,
      1,
      196500,
      453240,
      220,
      0,
      1,
      1,
      1159724,
      392670,
      0,
      2,
      806990,
      30482,
      4,
      1927926,
      82523,
      4,
      265318,
      0,
      4,
      0,
      85931,
      32,
      205665,
      812,
      1,
      1,
      41182,
      32,
      212342,
      32,
      31220,
      32,
      32696,
      32,
      43357,
      32,
      32247,
      32,
      38314,
      32,
      35892428,
      10,
      57996947,
      18975,
      10,
      38887044,
      32947,
      10,
    ],
  },
};
