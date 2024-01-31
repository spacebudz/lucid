# Issues to solve:

## Blockfrost Provider:
The function `getUtxosByPolicyId` gives, for some policy ids, a number of UTxOs different to the amount returned by the other two providers. For example, for the id `4d694d886f51c2142af20a97f0dc67d0113aa99762f3f33d7af6c17a`, it returns 84 UTxOs versus the 57 given by Maestro and Kupmios.

Also, for some policy ids, the previous function raises an error with status code 429, as is the case of the policy id `e16c2dc8ae937e8d3790c7fd7168d7b994621ba14ca11415f39fed72`.

## Maestro Provider:
There is a problem with the pagination of the results when calling the endpoint https://docs.gomaestro.org/Indexer-API/Transactions/txos-by-txo-refs to fetch transaction outputs by output reference. On January 29, 2024 this issue was validated and raised internally by the Maestro team.

In general, the amount of UTxOs returned when calling `getUtxosByPolicyId` matches the number returned by Kupmios, except for rare cases like the policy id `f2dbd5db5ad0571ea533411e99dc960f582c0debbb731d746079f6a0`, for which Maestro returns 82 UTxOs and Kupmios 81.
