import Core from 'core/types';
import {
  Address,
  ProtocolParameters,
  ProviderSchema,
  Slot,
  TxHash,
  Unit,
  UTxO,
} from '../types';
import axios from 'axios';
import { toHex } from '../utils';

export class Blockfrost implements ProviderSchema {
  url: string;
  projectId: string;
  constructor(url: string, projectId: string) {
    this.url = url;
    this.projectId = projectId;
  }

  async getProtocolParameters(): Promise<ProtocolParameters> {
    const { data: result } = await axios.get(
      `${this.url}/epochs/latest/parameters`,
      {
        headers: { project_id: this.projectId },
      },
    );

    return {
      minFeeA: parseInt(result.min_fee_a),
      minFeeB: parseInt(result.min_fee_b),
      maxTxSize: parseInt(result.max_tx_size),
      maxValSize: parseInt(result.max_val_size),
      keyDeposit: BigInt(result.key_deposit),
      poolDeposit: BigInt(result.pool_deposit),
      priceMem: parseFloat(result.price_mem),
      priceStep: parseFloat(result.price_step),
      coinsPerUtxoWord: BigInt(result.coins_per_utxo_word),
    };
  }
  async getCurrentSlot(): Promise<Slot> {
    return axios
      .get(`${this.url}/blocks/latest`, {
        headers: { project_id: this.projectId },
      })
      .then(({ data: res }) => parseInt(res.slot));
  }

  async getUtxos(address: string): Promise<UTxO[]> {
    let result = [];
    let page = 1;
    /*eslint no-constant-condition: ["error", { "checkLoops": false }]*/
    while (true) {
      let { data: pageResult } = await axios.get(
        `${this.url}/addresses/${address}/utxos?page=${page}`,
        {
          headers: { project_id: this.projectId },
        },
      );
      if (pageResult.error) {
        if ((result as any).status_code === 400) return [];
        else if ((result as any).status_code === 500) return [];
        else {
          pageResult = [];
        }
      }
      result = result.concat(pageResult);
      if (pageResult.length <= 0) break;
      page++;
    }
    return result.map((r) => ({
      txHash: r.tx_hash,
      outputIndex: r.output_index,
      assets: (() => {
        const a = {};
        r.amount.forEach((am: any) => {
          a[am.unit] = BigInt(am.quantity);
        });
        return a;
      })(),
      address,
      datumHash: r.data_hash,
    }));
  }

  async getUtxosWithUnit(address: Address, unit: Unit): Promise<UTxO[]> {
    let result = [];
    let page = 1;
    while (true) {
      let { data: pageResult } = await axios.get(
        `${this.url}/addresses/${address}/utxos/${unit}?page=${page}`,
        { headers: { project_id: this.projectId } },
      );
      if (pageResult.error) {
        if ((result as any).status_code === 400) return [];
        else if ((result as any).status_code === 500) return [];
        else {
          pageResult = [];
        }
      }
      result = result.concat(pageResult);
      if (pageResult.length <= 0) break;
      page++;
    }
    return result.map((r) => ({
      txHash: r.tx_hash,
      outputIndex: r.output_index,
      assets: (() => {
        const a = {};
        return r.amount.forEach((am: any) => {
          a[am.unit] = BigInt(am.quantity);
        });
      })(),
      address,
      datumHash: r.data_hash,
    }));
  }

  async awaitTx(txHash: TxHash): Promise<boolean> {
    return new Promise((res) => {
      const confirmation = setInterval(async () => {
        try {
          const { data: isConfirmed } = await axios.get(
            `${this.url}/txs/${txHash}`,
            {
              headers: { project_id: this.projectId },
            },
          );
          if (isConfirmed) {
            clearInterval(confirmation);
            res(true);
            return;
          }
        } catch (error) {
          // Not confirmed yet
        }
      }, 3000);
    });
  }

  async submitTx(tx: Core.Transaction): Promise<TxHash> {
    const { data: result } = await axios({
      url: `${this.url}/tx/submit`,
      headers: {
        'Content-Type': 'application/cbor',
        project_id: this.projectId,
      },
      method: 'POST',
      data: toHex(tx.to_bytes()),
    });
    return result;
  }
}
