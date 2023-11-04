import {
  Assets,
  assetsToValue,
  C,
  Constr,
  coreToUtxo,
  createCostModels,
  Data,
  datumJsonToCbor,
  fromHex,
  fromLabel,
  fromUnit,
  Lucid,
  MerkleTree,
  PROTOCOL_PARAMETERS_DEFAULT,
  SLOT_CONFIG_NETWORK,
  toHex,
  toLabel,
  toUnit,
  utxoToCore,
  valueToAssets,
} from "../src/mod.ts";
import {
  assert,
  assertEquals,
  assertNotEquals,
} from "https://deno.land/std@0.145.0/testing/asserts.ts";
import * as fc from "https://esm.sh/fast-check@3.1.1";

const privateKey = C.PrivateKey.generate_ed25519().to_bech32();
const lucid = await Lucid.new(undefined, "Preprod");

// Preprod testing parameters

const slotConfig = SLOT_CONFIG_NETWORK[lucid.network];

const protocolParameters = PROTOCOL_PARAMETERS_DEFAULT;
lucid.protocolParameters = protocolParameters;
lucid.slotConfig = slotConfig;

lucid.selectWalletFromPrivateKey(privateKey);

Deno.test("PaymentKeyHash length", async () => {
  const { paymentCredential } = lucid.utils.getAddressDetails(
    await lucid.wallet.address(),
  );
  if (paymentCredential) {
    assertEquals(fromHex(paymentCredential.hash).length, 28);
  } else {
    assert(paymentCredential);
  }
});

Deno.test("Address type", async () => {
  const {
    address: { bech32 },
  } = lucid.utils.getAddressDetails(await lucid.wallet.address());
  const enterpriseAddress = C.EnterpriseAddress.from_address(
    C.Address.from_bech32(bech32),
  )!
    .to_address()
    .to_bech32(undefined);
  assertEquals(bech32, enterpriseAddress);
  assertEquals(bech32, await lucid.wallet.address());
});

Deno.test("No reward address", async () => {
  const { stakeCredential } = lucid.utils.getAddressDetails(
    await lucid.wallet.address(),
  );
  assertEquals(stakeCredential, undefined);
  assertEquals(await lucid.wallet.rewardAddress(), null);
});

Deno.test("Switch wallet", async () => {
  const oldAddress = await lucid.wallet.address();

  const newPrivateKey = C.PrivateKey.generate_ed25519().to_bech32();
  lucid.selectWalletFromPrivateKey(newPrivateKey);

  const address = await lucid.wallet.address();
  assertNotEquals(oldAddress, address);
});

Deno.test("Wallet from utxos roundtrip (legacy utxos)", async () => {
  const rawUtxos = [
    "8282582040d1e106e587e6123aef50fa4347e34ec8c76f405ee9802ce90a345077b4e264018258390093de07f8feeda49b93c4cfd6817ebcfb7bfc4a0b18237c0196b205da3446b33ae55d27691cbbed080df5182262c307cdab62d3a4883f39df821a009a29bab82d581c126b8676446c84a5cd6e3259223b16a2314c5676b88ae1c1f8579a8fa144744d494e199d72581c3044e869070c8acf8f0ee6c87c285008fde6990f956a07cb9fd1e070a1444e616d6901581c34d1adbf3a7e95b253fd0999fb85e2d41d4121b36b834b83ac069ebba144414749581903ef581c370e055e07af274b588dc9bf3ef3203ec8b6830b7cdfaca9de38fc8fa146506c616e657401581c4122bca203030a36d8b605ab488335b1f2ee40dd7cd1454ce42f545ea143666c7901581c416e45fb08dc61a59a6bea82bde19e3014cef27ac4616c4f583adc15a144436f6f6c01581c4371ba767200e3e50e5dc1ff17f3f8d5154fa606534b03251a58e273a146436f6c6f727301581c46eea81aeed38d5e3595c4bf148a60f28006332ea9721a55748758f4a148456c657068616e7401581c54c1878edf4e6d49eab5b47b95c1bb9c6cf0622bf8dc5df1de6dc51fa1426a6f01581c55b2100ca1ac2f6ad955c5cc8c1c87d0f6155fd6ccb6857b7da5754ca145746f75636801581c57fca08abbaddee36da742a839f7d83a7e1d2419f1507fcbf3916522a54443484f431a0096dfcb444d494e541b0000000129f4a1764556414e494c1a013373b6465242455252591a3b9ac9d4465342455252591a3b9ac985581c5b7e2b5608c0f38eb186241f8b883d2e7bcad382f78c1e4e8993e513a14763616c76696e3101581c5c1c784a488bde15f9519142a9ee07886266598052f3dcc2105a6329a146526f636b657401581c60614c06bc93079c4e884db4dd966d2912aaae32a07a4506f749426ca1445472656501581c64834db34a8719177ee5ce1eda1c4cb91d2dca28bfc536073ba2176aa1445761766501581c648fd48f82c0a0687fb3d48178d0bcb3f0c5fe6cec68a1385129d4faa1444e616d6901581c68fdca4411f9a96df98e5517db3b49be01afc306655bf5e17ace78aea1477363656e65727901581c6998f4f9478186f3c182a4b334ef4e7d440658af06562ace55f74587a1444475636b01581c715b818224e0f83c747e8ab443a647ccf4dd2a6f0b2b37d7af8b3a60a149427574746572666c7901581c7509d2c7f0f3eb36d0336fcb7127bf3db1d5336583908d049e5a3716a1444e616d6901581c75ac2ca896ba71faee950eff23ad269b8dbd7c66e0806164ae74352ca14b526f636b6574416761696e01581c766bb5e84dc3438a5d7b2d686da04047f5c3ea981eac03de63071444a145576174657201581c83e1588347feaa9183030a82aa301b6a60b97c3844cdfd56b4bde133a144526f736501581c844c30a27b7756379ddae50e0dbe86c001d1a576d7adfdccc42c0b0da1444e616d6901581c868c245e1d99b32509542230fe744c120358353f195b6c19845134aca151526f636b6574202121202320242066736401581c890b7c122f3383fc859eecec99fa0ced3521b6e96f521a37de49e8baa1414b01581c9733e6e8f97a60b3a5c1ce6fe82f5c75b030766330bc5b4ea92b8b07a14a416e6f746865724170651842581c9d6eed0860acf0020af0d1f42c4c5eca26a2fb65aa09ed06bfe8d6ffa146666f7265737401581c9e6e63bbd52f0106716c6c920dcc9ad716ac9b001a18259eb20948e2a144436f6f6c01581c9ee64af489f0f8c6b64524988397b17269137d69830ae36c8b1530eba1414101581ca302467c86dfa2ae00e391745ba2de255ea2ec6f634e97a0a8c8bf27a1445374617201581ca380eb65f09337b3a562647823bc7edc9200f1fb8ff3bf1007263b69a1444a4f4f4f01581cbef61059036b3a7dbdb881d8239994f481d521c52908ad4c07421ce0a145506172697301581cc0355288172685f5732362b2bb4b8524223ad80b9cd97510b1460ef0a1414601581cc2f3a2c1af2d8cf25a89c4ed6c5c3bec4a4881341722ff42a31bf8cca1444d6f6f6e01581cc316c4d73ecb737ec21145a2dab42b4ad0dd5409118b482d7c601861a1445465737401581cc5d47df9211545b6b3bcb72e16654b1e46c9151fddbbe87ca0569e2ea14753636f6f74657201581cc921e0caf5ea1d4e313c96a4b416a91e085ba1355d9cd51e1d1e51f0a1414a01581ccf997119d2bf1fcbf73da2648e7eb047d4d0c5d8da04a426f46ee1a3a14472696e6701581cd311d3488cc4fef19d05634adce8534977a3bc6fc18136ad65df1d4fa1446c7120041a00b5b5b9581cd3840433dfd5720677fbcd3a4e14f44b249e3327fe1b8821f549bfd0a14349636501581ce3f3eb3f4f343fae935cea3a95f13cfba4eb7262919aec4b6d497d3aa1414901581ce7273c2510d45ec13847d2371a5289b2e3c132af29a1f8571bc15625a14f526f636b65744c61756e636831323301581cea15a43c13f242786c69c2ad448739430a666d66adf19c1701b1cbefa1414b01581cf334fced32a07a832b57d8795994e59e49c3d7dd8809748ff2eb3f0ea1434a4f4f01",
    "82825820109fbbedaa6d8c0ed0262411802ddaf9e8596396cb9e010248f52df65a7d0d87018258390093de07f8feeda49b93c4cfd6817ebcfb7bfc4a0b18237c0196b205da3446b33ae55d27691cbbed080df5182262c307cdab62d3a4883f39df1a1f069542",
  ];
  lucid.selectWalletFrom({
    address:
      "addr_test1qzfauplclmk6fxuncn8adqt7hnahhlz2pvvzxlqpj6eqtk35g6en4e2aya53ewldpqxl2xpzvtps0ndtvtf6fzpl880srm02gc",
    utxos: rawUtxos.map((raw) =>
      coreToUtxo(C.TransactionUnspentOutput.from_bytes(fromHex(raw)))
    ),
  });

  const rawUtxos_ = (await lucid.wallet.getUtxos()).map((utxo) =>
    toHex(utxoToCore(utxo).to_legacy_bytes())
  );

  assertEquals(rawUtxos, rawUtxos_);
});

Deno.test("Construct plutus data", () => {
  const data = Data.to(
    new Constr(1, [BigInt(1), "abcd", "deff", new Constr(0, [])]),
  );

  assertEquals(data, "d87a9f0142abcd42deffd87980ff");
  // == 122([1, h'ABCD', h'DEFF', 121([])])
});

Deno.test("Deserialize plutus data", () => {
  const data = "d87a9f0141ab41ded87980ff";
  const desData = Data.from(data);
  assertEquals(data, Data.to(desData));
});

Deno.test("(De)serialize plutus bigint data", () => {
  const data = 42n;
  const datum = Data.to(data);
  assertEquals(data, Data.from(datum));
});

Deno.test("(De)serialize map", () => {
  const m = new Map();
  m.set(2n, 1n);
  m.set("53706163654275647a", 2n);
  const datum = Data.to(m);
  assertEquals(m, Data.from(datum) as Map<unknown, unknown>);
});
Deno.test("More complex datum structure", () => {
  const data = [new Constr(1, [new Map([[2n, 3n]])])];
  const datum = Data.to(data);
  assertEquals(data, Data.from(datum));
});

Deno.test("json datum to cbor datum", () => {
  const jsonDatum = {
    fields: [
      {
        fields: [
          {
            bytes: "bdd5402a608267d8d47e021a61e5c1ae6aa62a1f770579aa38b88143",
          },
          {
            map: [
              {
                k: {
                  bytes: "",
                },
                v: {
                  map: [
                    {
                      k: {
                        bytes: "",
                      },
                      v: {
                        int: 49000000,
                      },
                    },
                  ],
                },
              },
            ],
          },
          {
            list: [
              {
                fields: [
                  {
                    bytes:
                      "bdd5402a608267d8d47e021a61e5c1ae6aa62a1f770579aa38b88143",
                  },
                  {
                    map: [
                      {
                        k: {
                          bytes: "",
                        },
                        v: {
                          map: [
                            {
                              k: {
                                bytes: "",
                              },
                              v: {
                                int: 49000000,
                              },
                            },
                          ],
                        },
                      },
                    ],
                  },
                ],
                constructor: 0,
              },
              {
                fields: [
                  {
                    bytes:
                      "6900ecb87083dbbe74a65b1036186bc9c12df2878842d936902f0b51",
                  },
                  {
                    map: [
                      {
                        k: {
                          bytes: "",
                        },
                        v: {
                          map: [
                            {
                              k: {
                                bytes: "",
                              },
                              v: {
                                int: 735000,
                              },
                            },
                          ],
                        },
                      },
                    ],
                  },
                ],
                constructor: 0,
              },
            ],
          },
          {
            int: 1653325108875,
          },
          {
            bytes: "f4a4a183be0b0da6e7a7548d1b26f2191b1ab7b2d20ac1c7d97b681c",
          },
        ],
        constructor: 0,
      },
    ],
    constructor: 0,
  };
  const cborDatum =
    "d8799fd8799f581cbdd5402a608267d8d47e021a61e5c1ae6aa62a1f770579aa38b88143a140a1401a02ebae409fd8799f581cbdd5402a608267d8d47e021a61e5c1ae6aa62a1f770579aa38b88143a140a1401a02ebae40ffd8799f581c6900ecb87083dbbe74a65b1036186bc9c12df2878842d936902f0b51a140a1401a000b3718ffff1b00000180f1db168b581cf4a4a183be0b0da6e7a7548d1b26f2191b1ab7b2d20ac1c7d97b681cffff";
  assertEquals(cborDatum, datumJsonToCbor(jsonDatum));
});

Deno.test("Assets to value", () => {
  const unit = "0".repeat(56);
  const assets = { lovelace: 5000000n, [unit]: 8n };

  const value = assetsToValue(assets);
  assertEquals(BigInt(value.coin().to_str()), assets.lovelace);
  assertEquals(value.multiasset()!.len(), 1);
});

Deno.test("Value to assets", () => {
  const value = C.Value.new(C.BigNum.from_str("5000000"));

  const assets = valueToAssets(value);
  assertEquals(BigInt(value.coin().to_str()), assets.lovelace);
});

Deno.test("Assets/value conversion property test", () => {
  fc.assert(
    fc.property(
      fc.array(
        fc.tuple(
          fc.uint8Array({ minLength: 28, maxLength: 28 }),
          fc.uint8Array({ minLength: 0, maxLength: 32 }),
          fc.bigInt({ min: 0n, max: 18446744073709551615n }),
        ),
      ),
      fc.bigInt({ min: 0n, max: 18446744073709551615n }),
      (
        assetsArray: Array<[Uint8Array, Uint8Array, bigint]>,
        lovelace: bigint,
      ) => {
        const assets: Assets = assetsArray.reduce(
          (acc, asset) => ({
            ...acc,
            [toHex(asset[0]) + toHex(asset[1])]: asset[2],
          }),
          {},
        );
        assets.lovelace = lovelace;

        assertEquals(assets, valueToAssets(assetsToValue(assets)));
      },
    ),
  );
});

Deno.test("Basic Merkle tree", () => {
  const data = [new Uint8Array([0]), new Uint8Array([1])];
  const merkleTree = new MerkleTree(data);
  const rootHash = merkleTree.rootHash();
  const proof = merkleTree.getProof(data[0]);
  assert(MerkleTree.verify(data[0], rootHash, proof));
  assertEquals(merkleTree.size(), 3);
});

Deno.test("Merkle tree property test", () => {
  fc.assert(
    fc.property(
      fc.array(fc.uint8Array(), { minLength: 1 }),
      (data: Uint8Array[]) => {
        const merkleTree = new MerkleTree(data);
        const rootHash = merkleTree.rootHash();
        const index = Math.floor(Math.random() * data.length);
        const proof = merkleTree.getProof(data[index]);
        assert(MerkleTree.verify(data[index], rootHash, proof));
        assertEquals(
          merkleTree.size(),
          Math.max(0, data.length + (data.length - 1)),
        );
      },
    ),
  );
});

Deno.test("PlutusData from JSON", () => {
  const plutusData = Data.fromJson({
    test: 123,
  });
  assertEquals(plutusData, new Map([["74657374", 123n]]));
});

Deno.test("PlutusData to JSON", () => {
  const json = Data.toJson(new Map([["74657374", 123n]]));
  assertEquals(json, {
    test: 123,
  });
});

Deno.test("JSON Metadata to CBOR Datum", () => {
  const cborDatum = Data.to(
    Data.fromJson({
      test: 123,
    }),
  );
  assertEquals(cborDatum, "a14474657374187b");
});

Deno.test("CBOR Datum to JSON Metadata", () => {
  const cborDatum = Data.toJson(Data.from("a14474657374187b"));
  assertEquals(cborDatum, {
    test: 123,
  });
});

Deno.test("toLabel/fromLabel property test", () => {
  fc.assert(
    fc.property(fc.integer({ min: -1, max: 65536 }), (n: number) => {
      if (n < 0 || n > 65535) {
        try {
          fromLabel(toLabel(n));
          assert(false);
        } catch (_e) {
          assert(true);
        }
      } else {
        assertEquals(n, fromLabel(toLabel(n)));
      }
    }),
  );
});

Deno.test("toUnit/fromUnit property test", () => {
  fc.assert(
    fc.property(
      fc.uint8Array({ minLength: 28, maxLength: 28 }),
      fc.uint8Array({ minLength: 0, maxLength: 10 }),
      fc.integer({ min: 0, max: 65535 }),
      (policyRaw: Uint8Array, nameRaw: Uint8Array, label: number) => {
        const policyId = toHex(policyRaw);
        const name = nameRaw.length > 0 ? toHex(nameRaw) : null;
        const assetName = toLabel(label) + (name || "");
        assertEquals(fromUnit(toUnit(policyId, name, label)), {
          policyId,
          assetName,
          name,
          label,
        });
      },
    ),
  );
});

Deno.test("Preserve task/transaction order", async () => {
  lucid.selectWalletFrom({
    address:
      "addr_test1qq90qrxyw5qtkex0l7mc86xy9a6xkn5t3fcwm6wq33c38t8nhh356yzp7k3qwmhe4fk0g5u6kx5ka4rz5qcq4j7mvh2sts2cfa",
    utxos: [
      {
        txHash:
          "2eefc93bc0dda80e78890f1f965733239e1f64f76555e8dcde1a4aa7db67b129",
        outputIndex: 3,
        assets: { lovelace: 6770556044n },
        address:
          "addr_test1qq90qrxyw5qtkex0l7mc86xy9a6xkn5t3fcwm6wq33c38t8nhh356yzp7k3qwmhe4fk0g5u6kx5ka4rz5qcq4j7mvh2sts2cfa",
        datumHash: null,
        datum: null,
        scriptRef: null,
      },
    ],
  });

  const txCompA = lucid
    .newTx()
    .payToAddressWithData(
      await lucid.wallet.address(),
      { inline: Data.to(0n) },
      {},
    );

  const txCompB = lucid
    .newTx()
    .payToAddressWithData(
      await lucid.wallet.address(),
      { inline: Data.to(10n) },
      {},
    )
    .compose(
      lucid
        .newTx()
        .payToAddressWithData(
          await lucid.wallet.address(),
          { inline: Data.to(1n) },
          {},
        )
        .compose(
          lucid
            .newTx()
            .payToAddressWithData(
              await lucid.wallet.address(),
              { inline: Data.to(2n) },
              {},
            ),
        ),
    );

  const tx = await lucid
    .newTx()
    .compose(txCompA)
    .compose(txCompB)
    .payToAddressWithData(
      await lucid.wallet.address(),
      { inline: Data.to(3n) },
      {},
    )
    .complete();

  [0n, 10n, 1n, 2n, 3n].forEach((num, i) => {
    const outputNum = BigInt(
      tx.txComplete
        .body()
        .outputs()
        .get(i)
        .datum()
        ?.as_data()
        ?.get()
        .as_integer()
        ?.to_str()!,
    );
    assertEquals(num, outputNum);
  });
});
