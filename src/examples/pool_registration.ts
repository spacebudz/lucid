import {
  Blockfrost,
  C,
  FreeableBucket,
  Freeables,
  fromHex,
  Lucid,
  PoolParams,
} from "../mod.ts";

/** When working with objects from the C module or objects that have fields we need to always free their memory when we're done to prevent memory leaks. A FreeableBucket is one way to do that.*/
const bucket: FreeableBucket = [];

const lucid = await Lucid.new(
  new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", "<projectId>"),
  "Preview"
);

lucid.selectWalletFromSeed("car rare ...");

/** StakePoolSigningKey_ed25519 cborHex from the cardano-cli */
const coldKey = C.PrivateKey.from_bytes(
  fromHex(
    "58204de30f983ed860524d00059c7f2b1d63240fba805bee043604aa7ccb13d387e9"
  )
);
bucket.push(coldKey);

/** VrfVerificationKey_PraosVRF cborHex from the cardano-cli */
const vrfVKey = C.VRFVKey.from_bytes(
  fromHex(
    "5820c9cf07d863c8a2351662c9759ca1d9858b536bab50ad575b5de161e1af18f887"
  )
);
bucket.push(vrfVKey);

const vrfVKeyHash = vrfVKey.hash();
bucket.push(vrfVKeyHash);
const vrfKeyHash = vrfVKeyHash.to_hex();

const publicKey = coldKey.to_public();
bucket.push(publicKey);
const publicKeyHash = publicKey.hash();
bucket.push(publicKeyHash);
const poolId = publicKeyHash.to_bech32("pool");

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

const tx = lucid.newTx().registerPool(poolParams);
bucket.push(tx);
const completeTX = await lucid.newTx().registerPool(poolParams).complete();
bucket.push(completeTX);

const signedTx = await completeTX
  .sign()
  .signWithPrivateKey(coldKey.to_bech32())
  .complete();
bucket.push(signedTx);

const txHash = await signedTx.submit();

Freeables.free(...bucket);

console.log(txHash);
