import { fromText, Lucid } from "../../mod.ts";

// Sign a message and verify it.

const lucid = await Lucid.new();

lucid.selectWalletFromSeed("car rare ...");

const address = await lucid.wallet.address();
const payload = fromText("Hello from Lucid!");

const signedMessage = await lucid.newMessage(address, payload).sign();

// Verify the message

const hasSigned: boolean = lucid.verifyMessage(address, payload, signedMessage);

console.log(hasSigned);
