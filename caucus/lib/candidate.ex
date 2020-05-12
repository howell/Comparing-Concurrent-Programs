defmodule Caucus.Candidate do
  use GenServer

  defstruct name: "", tax_rate: 0

  def start_link(cand_struct) do
    GenServer.start_link(__MODULE__, cand_struct, name: cand_struct[:name])
  end

  def init(cand_struct) do
    {:ok, cand_struct, {:continue, :init}}
  end

  def handle_continue(:init, state) do
    Caucus.CandidateRegistry.register_cand(state)
    {:noreply, state}
  end
end
