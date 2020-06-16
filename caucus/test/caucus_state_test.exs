defmodule CandidateState do
  use ExUnit.Case
  use PropCheck
  use PropCheck.StateM

  defmodule CandidateInterface do
    def ballot(pid, ballot) do
      send pid, {:ballot, ballot}
    end
  end

  def initial_state do
    let {name, threshold} <- {char_list(), nat()} do
      %{pid: Candidate.spawn(name, any(), threshold, self()), name: name, threshold: threshold}
    end
  end

  def command(%{pid: pid, name: name, threshold: num}) do
    let votes <- range(num + 1, :inf) do
      {:call, CandidateInterface, :ballot, [pid, %{name => votes}]}
    end
  end

  def precondition(_, {:call, _, _, [_, map]}) do
    [h | _] = Map.values(map)
    Kernel.map_size(map) == 1 && h > 5
  end

  def postcondition(_, _, _), do: true

  def next_state(s, _, _), do: s

  property "Will not withdraw from race if gets enough votes" do
    forall cmds <- commands(__MODULE__) do
      {_, _, res} = run_commands(__MODULE__, cmds)
      res == :ok
    end
  end
end

"""
defmodule RegistryState do
  defmodule RegistryInterface do
  end

  def initial_state do
    pid = AbstractRegistry.create(Voter)
    %{type: Voter, data: MapSet.new, subscribers: MapSet.new}
  end

  def command(%{type: type, data: data, subscribers: subscribers}) do
    oneof([
    ])

    # types of commands:
    # 1. somebody publishes a voter struct
    # 2. somebody subscribes
    # 3. somebody removes a struct from the registry--should only work if it exists
    # 4. somebody sends a msg request
  end
end
"""
