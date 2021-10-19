# ICP Staking Forecaster

To use (assuming you have the `jq` utility installed):

```
nix-shell
cabal configure
cabal build
$(find . -name icp-forecast -type f -executable) \
    --start 0.41                                 \
    --voting 67                                  \
    --minting 5                                  \
    --stake 100000                               \
    --delay 4                                    \
    --dissolve 4                                 \
    --duration 8                                 \
    --compound True | jq -r
```

Some notes on the meaning of the parameters, all of which are currently
necessary and in the above order:

`--start` is the number of years since genesis that calculation should begin
at. `0.41` is five months.

`--voting` is the percentage of supply that reflects the current voting power.
This can be greater than 100.

`--minting` is the percentage of rewards that are minted by spawning each day.

`--stake` is the initial stake of the hypothetical neuron.

`--delay` is the dissolve delay of that neuron, in years. Decimals are allowed.

`--dissolve` is when to start dissolving, in years. So, a delay of 4 with a
dissolve of 4 years means that the neuron has a dissolve delay of 4 years, but
it will not start dissolving until 4 years have transpired.

`--duration` is how long to run the simulator for. This number can be shorter
than either of `--delay` or `--dissolve`, in which case it simply means that
the delay period may not expire or start dissolving before the simulation
ends.

`--compound` indicates whether rewards should always be compounded back into
the original neuron.
