import { getAddressDetails, Lucid, S } from '../src';

const privateKey = S.PrivateKey.generate_ed25519().to_bech32();
await Lucid.selectWalletFromPrivateKey(privateKey);

describe('Testing wallet', () => {
  test('PaymentKeyHash length', async () => {
    const { paymentKeyHash } = getAddressDetails(Lucid.wallet.address);
    expect(Buffer.from(paymentKeyHash, 'hex')).toHaveLength(28);
  });

  test('Address type', async () => {
    const { address } = getAddressDetails(Lucid.wallet.address);
    const enterpriseAddress = S.EnterpriseAddress.from_address(
      S.Address.from_bech32(address),
    )
      .to_address()
      .to_bech32();
    expect(address).toBe(enterpriseAddress);
    expect(address).toBe(Lucid.wallet.address);
  });

  test('No reward address', async () => {
    const { stakeKeyHash } = getAddressDetails(Lucid.wallet.address);
    expect(stakeKeyHash).toBeUndefined();
    expect(Lucid.wallet.rewardAddress).toBeUndefined();
  });

  test('Switch wallet', async () => {
    const oldAddress = Lucid.wallet.address;

    const newPrivateKey = S.PrivateKey.generate_ed25519().to_bech32();
    await Lucid.selectWalletFromPrivateKey(newPrivateKey);

    const address = Lucid.wallet.address;
    expect(oldAddress).not.toBe(address);
  });
});
