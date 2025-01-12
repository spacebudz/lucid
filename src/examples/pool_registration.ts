import {
  Blockfrost,
  Crypto,
  Hasher,
  Lucid,
  PoolRegistration,
  Utils,
} from "../mod.ts";

const lucid = new Lucid({
  provider: new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    "<projectId>",
  ),
}).selectWalletFromSeed("car rare ...");

/** StakePoolSigningKey_ed25519 cborHex from the cardano-cli */
const { privateKey: coldKey, credential: coldCredential } = Crypto
  .privateKeyToDetails(
    "58204de30f983ed860524d00059c7f2b1d63240fba805bee043604aa7ccb13d387e9",
  );

/** VrfVerificationKey_PraosVRF cborHex from the cardano-cli */
const vrfKeyHash = Hasher.hashVrfKey(
  "5820c9cf07d863c8a2351662c9759ca1d9858b536bab50ad575b5de161e1af18f887",
);

const poolId = Utils.encodeBech32("pool", coldCredential.hash);

const rewardOwnerAddress = (await lucid.wallet.rewardAddress())!;

const poolParams: PoolRegistration = {
  poolId,
  vrfKeyHash,
  pledge: 100000000,
  cost: 340000000,
  margin: 0.025, // 2.5%
  rewardAddress: rewardOwnerAddress,
  owners: [rewardOwnerAddress],
  relays: [{
    type: "SingleHostIp",
    ipV4: "123.456.789.0",
    ipV6: undefined,
    port: 3000,
  }],
  metadataUrl: "https://...", // metadata needs to be hosted already before registering the pool
};

const tx = await lucid.newTx()
  .registerPool(poolParams).commit();

const signedTx = await tx.sign()
  .signWithPrivateKey(coldKey)
  .commit();

const txHash = await signedTx.submit();

console.log(txHash);
