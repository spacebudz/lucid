# Issues to solve:

## Blockfrost Provider:
The function `getUtxosByPolicyId` gives, for some policy ids, a number of UTxOs different to the amount returned by the other two providers. For example, for the id `4d694d886f51c2142af20a97f0dc67d0113aa99762f3f33d7af6c17a`, it returns 84 UTxOs versus the 57 given by Maestro and Kupmios.

## Maestro Provider:
There is a problem with the pagination of the results when calling the endpoint https://docs.gomaestro.org/Indexer-API/Transactions/txos-by-txo-refs to fetch transaction outputs by output reference. On January 29, 2024 this issue was validated and raised internally by the Maestro team.
