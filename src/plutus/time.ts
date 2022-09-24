import { Network, Slot, SlotConfig, UnixTime } from "../types/mod.ts";

export const zeroTimeNetwork: Record<Network, UnixTime> = {
  Mainnet: 1596049091000, // Shelley start (slotLength = 1s).
  Testnet: 1595967616000, // Shelley start (slotLength = 1s).
  Preview: 1660003200000, // Genesis start (slotLength = 1s).
  Preprod: 1654041600000, // Genesis start (slotLength = 1s).
};

/** Currently the slot length in each era (after Byron) is 1 second. */
export const DEFAULT_SLOT_LENGTH = 1000;

export function slotToBeginUnixTime(
  slot: Slot,
  slotConfig: SlotConfig,
): UnixTime {
  const msAfterBegin = slot * slotConfig.slotLength;
  return slotConfig.zeroTime + msAfterBegin;
}

// slotToBeginUnixTime and slotToEndUnixTime are identical when slotLength == 1. So we don't need to worry about this now.
// function slotToEndUnixTime(slot: Slot, slotConfig: SlotConfig): UnixTime {
//   return slotToBeginUnixTime(slot, slotConfig) + (slotConfig.slotLength - 1);
// }

export function unixTimeToEnclosingSlot(
  unixTime: UnixTime,
  slotConfig: SlotConfig,
): Slot {
  const timePassed = unixTime - slotConfig.zeroTime;
  const slotsPassed = Math.floor(timePassed / slotConfig.slotLength);
  return slotsPassed;
}
