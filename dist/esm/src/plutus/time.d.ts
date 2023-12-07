import { Network, Slot, SlotConfig, UnixTime } from "../types/mod.js";
export declare const SLOT_CONFIG_NETWORK: Record<Network, SlotConfig>;
export declare function slotToBeginUnixTime(slot: Slot, slotConfig: SlotConfig): UnixTime;
export declare function unixTimeToEnclosingSlot(unixTime: UnixTime, slotConfig: SlotConfig): Slot;
