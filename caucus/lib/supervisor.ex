defmodule Caucus.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_init_arg) do
    children = [
      Caucus.CandidateRegistry,
      Supervisor.child_spec({Caucus.Candidate, ["Bernie", 50]}, id: "Bernie")
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
