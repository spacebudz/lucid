import { getAddressDetails, Lucid, S } from '../src';

describe('test', () => {
  test('Address details', async () => {
    const privateKey = S.PrivateKey.generate_ed25519().to_bech32();
    await Lucid.selectWalletFromPrivateKey(privateKey);
    const { paymentKeyHash } = getAddressDetails(Lucid.wallet.address);
    expect(paymentKeyHash).toBeDefined();
  });
});
