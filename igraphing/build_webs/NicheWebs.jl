using CSV
using DataFrames
using Extinctions
using SpeciesInteractionNetworks

# set seed
import Random
Random.seed!(66)

# set Co range
Co_range = [0.01, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2, 0.225]

# edgelist df
edgelist_df = DataFrame(net_id=Any[], network=Any[], connectance=Any[], consumer=Any[], resource=Any[]);

# build Niche webs

for (i, Co) in enumerate(Co_range)

    N = structuralmodel(NicheModel, 20, Co)

    intxns = interactions(N)

    d = DataFrame(
        net_id=fill(i, length(intxns)),
        network=N,
        connectance=Co,
        consumer=getindex.(intxns, 1),
        resource=getindex.(intxns, 2)
    )

    append!(edgelist_df, d)

end


# export edgelists
CSV.write("edgelists.csv",
    edgelist_df[!, Not(:network)])

# Do ~ 100 robustness sims for each web 

spread = collect(1:99)
n_reps = 100

robustness_vals = DataFrame(
    net_id=Any[],
    threshold=Int[],
    robustness=Float64[],
)

for _ in 1:n_reps

    for j in 1:nrow(edgelist_df)

        N = edgelist_df.network[j]

        for threshold in spread

            rob = robustness(
                N;
                threshold=threshold,
                mechanism=:cascade,
                remove_disconnected=true
            )

            push!(robustness_vals, (
                net_id=edgelist_df.net_id[j],
                threshold=threshold,
                robustness=rob,
            ))

        end
    end
end

CSV.write("robustness.csv", robustness_vals)