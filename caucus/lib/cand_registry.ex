defmodule Caucus.CandidateRegistry do
  use GenServer
  require Logger

  defstruct pub_data: MapSet.new(), subs: MapSet.new()

  def start_link(_init_arg) do
    GenServer.start_link(__MODULE__, [], name: {:global, :cand_reg})
  end

  def init(_init_arg) do
    {:ok, %Caucus.CandidateRegistry{}} 
  end

  def register_cand(cand_struct) do
    GenServer.cast(:cand_reg, cand_struct)
  end

  def handle_cast(%Caucus.Candidate{name: name, ...}, _from, %Caucus.CandidateRegistry{pub_data: ...}) do
    {
      :noreply, 
      %Caucus.CandidateRegistry{
        pub_data: MapSet.put(MapSet.new(), %Caucus.Candidate{}), 
        subs: MapSet.new()
      }
    }
  end
end
