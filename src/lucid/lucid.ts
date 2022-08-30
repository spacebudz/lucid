import { C, Core } from "../core/mod.ts";
import {
  coreToUtxo,
  createCostModels,
  fromHex,
  toHex,
  Utils,
  utxoToCore,
} from "../utils/mod.ts";
import {
  Address,
  Datum,
  ExternalWallet,
  Network,
  OutRef,
  PrivateKey,
  Provider,
  Slot,
  Transaction,
  TxHash,
  Unit,
  UTxO,
  Wallet,
  WalletApi,
} from "../types/mod.ts";
import { Tx } from "./tx.ts";
import { TxComplete } from "./txComplete.ts";

export class Lucid {
  txBuilderConfig!: Core.TransactionBuilderConfig;
  wallet!: Wallet;
  provider!: Provider;
  network: Network = "Mainnet";
  utils!: Utils;

  static async new(provider?: Provider, network?: Network) {
    const lucid = new this();
    if (network) lucid.network = network;
    if (provider) {
      lucid.provider = provider;
      const protocolParameters = await provider.getProtocolParameters();
      lucid.txBuilderConfig = C.TransactionBuilderConfigBuilder.new()
        .coins_per_utxo_byte(
          C.BigNum.from_str(protocolParameters.coinsPerUtxoByte.toString()),
        )
        .fee_algo(
          C.LinearFee.new(
            C.BigNum.from_str(protocolParameters.minFeeA.toString()),
            C.BigNum.from_str(protocolParameters.minFeeB.toString()),
          ),
        )
        .key_deposit(
          C.BigNum.from_str(protocolParameters.keyDeposit.toString()),
        )
        .pool_deposit(
          C.BigNum.from_str(protocolParameters.poolDeposit.toString()),
        )
        .max_tx_size(protocolParameters.maxTxSize)
        .max_value_size(protocolParameters.maxValSize)
        .collateral_percentage(protocolParameters.collateralPercentage)
        .max_collateral_inputs(protocolParameters.maxCollateralInputs)
        .ex_unit_prices(
          C.ExUnitPrices.from_float(
            protocolParameters.priceMem,
            protocolParameters.priceStep,
          ),
        )
        .blockfrost(
          // Provider needs to be blockfrost in this case. Maybe we have better/more ways in the future to evaluate ex units
          C.Blockfrost.new(
            provider.data.url + "/utils/txs/evaluate",
            provider.data.projectId,
          ),
        )
        .costmdls(createCostModels(protocolParameters.costModels))
        .build();
    }
    lucid.utils = new Utils(lucid);
    return lucid;
  }

  newTx(): Tx {
    return new Tx(this);
  }

  fromTx(tx: Transaction) {
    return new TxComplete(this, C.Transaction.from_bytes(fromHex(tx)));
  }

  currentSlot(): Promise<Slot> {
    return this.provider.getCurrentSlot();
  }

  utxosAt(address: Address): Promise<UTxO[]> {
    return this.provider.getUtxos(address);
  }

  utxosAtWithUnit(address: Address, unit: Unit): Promise<UTxO[]> {
    return this.provider.getUtxosWithUnit(address, unit);
  }

  utxosByOutRef(outRefs: Array<OutRef>): Promise<UTxO[]> {
    return this.provider.getUtxosByOutRef(outRefs);
  }

  awaitTx(txHash: TxHash): Promise<boolean> {
    return this.provider.awaitTx(txHash);
  }

  async datumOf(utxo: UTxO): Promise<Datum> {
    if (utxo.datum) return utxo.datum;
    if (!utxo.datumHash) {
      throw new Error("This UTxO does not have a datum hash.");
    }
    utxo.datum = await this.provider.getDatum(utxo.datumHash);
    return utxo.datum;
  }

  /**
   * Cardano Private key in bech32; not the BIP32 private key or any key that is not fully derived
   */
  selectWalletFromPrivateKey(privateKey: PrivateKey) {
    const priv = C.PrivateKey.from_bech32(privateKey);
    const pubKeyHash = priv.to_public().hash();

    this.wallet = {
      // deno-lint-ignore require-await
      address: async () =>
        C.EnterpriseAddress.new(
          this.network === "Mainnet" ? 1 : 0,
          C.StakeCredential.from_keyhash(pubKeyHash),
        )
          .to_address()
          .to_bech32(undefined),
      // deno-lint-ignore require-await
      rewardAddress: async () => undefined,
      getUtxos: async () => {
        return await this.utxosAt(await this.wallet.address());
      },
      getUtxosCore: async () => {
        const utxos = await this.utxosAt(await this.wallet.address());
        const coreUtxos = C.TransactionUnspentOutputs.new();
        utxos.forEach((utxo) => {
          coreUtxos.add(utxoToCore(utxo));
        });
        return coreUtxos;
      },
      // deno-lint-ignore require-await
      signTx: async (tx: Core.Transaction) => {
        const witness = C.make_vkey_witness(
          C.hash_transaction(tx.body()),
          priv,
        );
        const txWitnessSetBuilder = C.TransactionWitnessSetBuilder.new();
        txWitnessSetBuilder.add_vkey(witness);
        return txWitnessSetBuilder.build();
      },
      submitTx: async (tx: Core.Transaction) => {
        return await this.provider.submitTx(tx);
      },
    };
    return this;
  }

  selectWallet(api: WalletApi) {
    this.wallet = {
      address: async () =>
        C.Address.from_bytes(
          fromHex((await api.getUsedAddresses())[0]),
        ).to_bech32(undefined),
      rewardAddress: async () => {
        const [rewardAddressHex] = await api.getRewardAddresses();
        const rewardAddress = rewardAddressHex ??
          C.RewardAddress.from_address(
            C.Address.from_bytes(fromHex(rewardAddressHex)),
          )!
            .to_address()
            .to_bech32(undefined);
        return rewardAddress;
      },
      getUtxos: async () => {
        const utxos = ((await api.getUtxos()) || []).map((utxo) => {
          const parsedUtxo = C.TransactionUnspentOutput.from_bytes(
            fromHex(utxo),
          );
          return coreToUtxo(parsedUtxo);
        });
        return utxos;
      },
      getUtxosCore: async () => {
        const utxos = C.TransactionUnspentOutputs.new();
        ((await api.getUtxos()) || []).forEach((utxo) => {
          utxos.add(C.TransactionUnspentOutput.from_bytes(fromHex(utxo)));
        });
        return utxos;
      },
      signTx: async (tx: Core.Transaction) => {
        const witnessSet = await api.signTx(toHex(tx.to_bytes()), true);
        return C.TransactionWitnessSet.from_bytes(fromHex(witnessSet));
      },
      submitTx: async (tx: Core.Transaction) => {
        const txHash = await api.submitTx(toHex(tx.to_bytes()));
        return txHash;
      },
    };
    return this;
  }

  /**
   * Emulates a CIP30 wallet by constructing it
   * with the UTxOs and an address.
   *
   * If utxos are not set, utxos are fetched from the provided address
   */
  selectWalletFrom({
    address,
    utxos,
    rewardAddress,
  }: ExternalWallet) {
    const addressDetails = this.utils.getAddressDetails(address);
    this.wallet = {
      // deno-lint-ignore require-await
      address: async () => address,
      // deno-lint-ignore require-await
      rewardAddress: async () => {
        const rewardAddr = !rewardAddress && addressDetails.stakeCredential
          ? (() => {
            if (addressDetails.stakeCredential.type === "Key") {
              return C.RewardAddress.new(
                this.network === "Mainnet" ? 1 : 0,
                C.StakeCredential.from_keyhash(
                  C.Ed25519KeyHash.from_hex(
                    addressDetails.stakeCredential.hash,
                  ),
                ),
              )
                .to_address()
                .to_bech32(undefined);
            }
            return C.RewardAddress.new(
              this.network === "Mainnet" ? 1 : 0,
              C.StakeCredential.from_scripthash(
                C.ScriptHash.from_hex(addressDetails.stakeCredential.hash),
              ),
            )
              .to_address()
              .to_bech32(undefined);
          })()
          : rewardAddress;
        return rewardAddr;
      },
      getUtxos: async () => {
        return utxos ? utxos : await this.utxosAt(address);
      },
      getUtxosCore: async () => {
        const coreUtxos = C.TransactionUnspentOutputs.new();
        (utxos ? utxos : await this.utxosAt(address)).forEach((utxo) =>
          coreUtxos.add(utxoToCore(utxo))
        );
        return coreUtxos;
      },
      // deno-lint-ignore require-await
      signTx: async (_: Core.Transaction) => {
        throw new Error("Not implemented");
      },
      submitTx: async (tx: Core.Transaction) => {
        return await this.provider.submitTx(tx);
      },
    };
    return this;
  }
}
