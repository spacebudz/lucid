export const SLOT_CONFIG_NETWORK = {
    Mainnet: { zeroTime: 1596059091000, zeroSlot: 4492800, slotLength: 1000 },
    Preview: { zeroTime: 1666656000000, zeroSlot: 0, slotLength: 1000 },
    Preprod: {
        zeroTime: 1654041600000 + 1728000000,
        zeroSlot: 86400,
        slotLength: 1000,
    },
    /** Customizable slot config (Initialized with 0 values). */
    Custom: { zeroTime: 0, zeroSlot: 0, slotLength: 0 },
};
export function slotToBeginUnixTime(slot, slotConfig) {
    const msAfterBegin = (slot - slotConfig.zeroSlot) * slotConfig.slotLength;
    return slotConfig.zeroTime + msAfterBegin;
}
// slotToBeginUnixTime and slotToEndUnixTime are identical when slotLength == 1. So we don't need to worry about this now.
// function slotToEndUnixTime(slot: Slot, slotConfig: SlotConfig): UnixTime {
//   return slotToBeginUnixTime(slot, slotConfig) + (slotConfig.slotLength - 1);
// }
export function unixTimeToEnclosingSlot(unixTime, slotConfig) {
    const timePassed = unixTime - slotConfig.zeroTime;
    const slotsPassed = Math.floor(timePassed / slotConfig.slotLength);
    return slotsPassed + slotConfig.zeroSlot;
}
