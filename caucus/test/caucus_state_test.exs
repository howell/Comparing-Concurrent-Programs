defmodule RegistryStateProps do
  use ExUnit.Case
  use PropCheck
  use PropCheck.StateM

  defmodule CandidateInterface do
    def ballot(pid, ballot) do
      send pid, {:ballot, ballot}
    end
  end

  defmodule CandidateState do
    def initial_state do
      generator = char_list()
      IO.puts inspect(generator)
      name = generator
      threshold = 5
      %{pid: Candidate.spawn(name, any(), threshold, self()), name: name, threshold: threshold}
    end

    def command(%{pid: pid, name: name, threshold: num}) do
      {:call, CandidateInterface, :ballot, [pid, %{name => 6}]}
    end

    def command({:call, CandidateInterface, :ballot, [arg1 | [arg2 | []]]}) do
      CandidateInterface.ballot(arg1, arg2)
    end

    def command(_), do: true

    def precondition(_, _), do: true
    def postcondition(_, _, _), do: true

    def next_state(s, _, _), do: s
  end

  property "Will not withdraw from race if gets enough votes" do
    forall cmds <- commands(CandidateState) do
      {_, _, res} = run_commands(CandidateState, cmds)
      res == :ok
    end
  end
end
