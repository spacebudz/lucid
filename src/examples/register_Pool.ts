import { Blockfrost, C, fromHex, Lucid, PoolParams } from "../mod.ts";

const lucid = await Lucid.new(
  new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", "<projectId>"),
  "Preview",
);

lucid.selectWalletFromSeed("car rare ...");

/** StakePoolSigningKey_ed25519 cborHex from the cardano-cli */
const coldKey = C.PrivateKey.from_bytes(
  fromHex(
    "58204de30f983ed860524d00059c7f2b1d63240fba805bee043604aa7ccb13d387e9",
  ),
);

/** VrfVerificationKey_PraosVRF cborHex from the cardano-cli */
const vrfKeyHash = C.VRFVKey.from_bytes(
  fromHex(
    "5820c9cf07d863c8a2351662c9759ca1d9858b536bab50ad575b5de161e1af18f887",
  ),
).hash().to_hex();

const poolId = coldKey.to_public().hash().to_bech32("pool");

const rewardOwnerAddress = (await lucid.wallet.rewardAddress())!;

const poolParams: PoolParams = {
  poolId,
  vrfKeyHash,
  pledge: 100000000n,
  cost: 340000000n,
  margin: 0.025, // 2.5%
  rewardAddress: rewardOwnerAddress,
  owners: [rewardOwnerAddress],
  relays: [{ type: "SingleHostIp", ipV4: "123.456.789.0", port: 3000 }],
  metadataUrl: "https://...", // metadata needs to be hosted already before registering the pool
};

const tx = await lucid.newTx()
  .registerPool(poolParams).complete();

const signedTx = await tx.sign()
  .signWithPrivateKey(coldKey.to_bech32())
  .complete();

const txHash = await signedTx.submit();

console.log(txHash);
