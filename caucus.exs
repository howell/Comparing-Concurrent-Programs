defmodule Candidate do
  defstruct name: "", tax_rate: 0

  def spawn(name, tax_rate, cand_registry) do
    spawn fn -> send cand_registry, %Candidate{name: name, tax_rate: tax_rate} end
  end
end

defmodule CandidateRegistry do
  def spawn do
    spawn fn -> loop(MapSet.new()) end 
  end

  defp loop(set) do
    receive do
      %Candidate{name: n, tax_rate: tr} -> 
        IO.puts n
        loop(MapSet.put(set, %Candidate{name: n, tax_rate: tr}))
    end
  end
end

cand_registry = CandidateRegistry.spawn

Candidate.spawn("Bernie", 50, cand_registry)
Candidate.spawn("Biden", 25, cand_registry)
Candidate.spawn("Tulsi", 10, cand_registry)
