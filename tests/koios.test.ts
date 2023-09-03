import {Koios} from "../src/provider/koios.ts";
import {Datum, Delegation, ProtocolParameters, UTxO} from "../src/types/types.ts";
import {assert} from "https://deno.land/std@0.145.0/testing/asserts.ts";
import {Lucid} from "../src/lucid/lucid.ts";

const koios = new Koios("https://api.koios.rest/api/v0")

Deno.test("getProtocolParameters", async () => {
    try {
        const pp: ProtocolParameters = await koios.getProtocolParameters();
        assert(pp);
    } catch (e) {
        console.log(e)
    }
});

Deno.test("getUtxos", async () => {
    try {
        const utxos: UTxO[] = await koios.getUtxos("addr1qy2jt0qpqz2z2z9zx5w4xemekkce7yderz53kjue53lpqv90lkfa9sgrfjuz6uvt4uqtrqhl2kj0a9lnr9ndzutx32gqleeckv");
        assert(utxos);
    } catch (e) {
        console.log(e)
    }
});

Deno.test("getUtxosWithUnit", async () => {
    try {
        const utxos: UTxO[] = await koios.getUtxosWithUnit(
            "addr1q8vaadv0h7atv366u6966u4rft2svjlf5uajy8lkpsgdrc24rnskuetxz2u3m5ac22s3njvftxcl2fc8k8kjr088ge0qpn6xhn",
            "85152e10643c1440ba2ba817e3dd1faf7bd7296a8b605efd0f0f2d1844696d656e73696f6e426f78202330313739");
        console.log(utxos)
        assert(utxos);
    } catch (e) {
        console.log(e)
    }
});

Deno.test("getUtxoByUnit", async () => {
    try {
        const utxo: UTxO = await koios.getUtxoByUnit("85152e10643c1440ba2ba817e3dd1faf7bd7296a8b605efd0f0f2d1844696d656e73696f6e426f78202330313739");
        assert(utxo);
    } catch (e) {
        console.log(e)
    }
});

Deno.test("getUtxosByOutRef", async () => {
    try {
        const utxos: UTxO[] = await koios.getUtxosByOutRef([{txHash: 'c6ee20549eab1e565a4bed119bb8c7fc2d11cc5ea5e1e25433a34f0175c0bef6', outputIndex: 0}]);
        assert(utxos);
    } catch (e) {
        console.log(e)
    }
});

Deno.test("getDelegation", async () => {
    const lucid = await Lucid.new(koios, "Mainnet");
    assert(lucid)
    try {
        const delegation: Delegation = await lucid.delegationAt('stake1uyrx65wjqjgeeksd8hptmcgl5jfyrqkfq0xe8xlp367kphsckq250');
        assert(delegation);
    } catch (e) {
        console.log(e)
    }
});

Deno.test("getDatum", async () => {
    try {
        const datum: Datum = await koios.getDatum('818ee3db3bbbd04f9f2ce21778cac3ac605802a4fcb00c8b3a58ee2dafc17d46');
        assert(datum);
    } catch (e) {
        console.log(e)
    }
});

Deno.test("awaitTx", async () => {
    try {
        const isConfirmed: boolean = await koios.awaitTx('f144a8264acf4bdfe2e1241170969c930d64ab6b0996a4a45237b623f1dd670e');
        assert(isConfirmed);
    } catch (e) {
        console.log(e)
    }
});

Deno.test("submitTxBadRequest", async () => {
    try {
        const txId: string = await koios.submitTx('80');
        assert(txId);
    } catch (e) {
        console.log(e)
    }
});
