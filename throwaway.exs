defmodule VoterRegistry do
  defstruct [:voters]

  # initialize a new VoterRegistry
  # -> PID
  def spawn do
    spawn fn -> loop(MapSet.new(), MapSet.new()) end
  end

  # Listen to messages and respond accordingly
  # [Setof Voter] [Setof PID] -> void
  defp loop(voters, subscribers) do
    receive do
      %Voter{name: name, pid: voter_pid} -> 
        IO.puts "Voter #{name} has registered!"
        new_voters = MapSet.put(voters, %Voter{name: name, pid: voter_pid})
        Enum.each(subscribers, fn s -> send s, %VoterRegistry{voters: new_voters} end)
        loop(new_voters, subscribers)
      {:subscribe, pid} ->
        IO.puts "New subscriber to the voter registry: #{inspect pid}!"
        send pid, %VoterRegistry{voters: voters}
        loop(voters, MapSet.put(subscribers, pid))
    end
  end
end

defmodule CandidateRegistry do
  defstruct [:candidates]

  # Start the actor
  # -> PID
  def spawn do
    spawn fn -> loop(MapSet.new(), MapSet.new()) end 
  end

  # Listen to messages from other actors and respond accordingly
  # [Setof Candidate] [Setof PID] -> void
  defp loop(candidates, subscribers) do
    receive do
      %Candidate{name: n, tax_rate: tr, pid: pid} -> 
        IO.puts "Candidate #{n} has registered!"
        new_candidates = MapSet.put(candidates, %Candidate{name: n, tax_rate: tr, pid: pid})
        Enum.each(subscribers, fn s -> send s, %CandidateRegistry{candidates: new_candidates} end)
        loop(new_candidates, subscribers)
      {:subscribe, pid} ->
        IO.puts "Subscriber #{inspect pid} has subscribed!"
        send pid, %CandidateRegistry{candidates: candidates}
        loop(candidates, MapSet.put(subscribers, pid))
      {:msg, pid} ->
        IO.puts "Process #{inspect pid} is requesting a message!"
        send pid, %CandidateRegistry{candidates: candidates}
        loop(candidates, subscribers)
      {:drop_out, cand} ->
        IO.puts "Candidate #{cand.name} is dropping out!"
        loop(MapSet.delete(candidates, cand), subscribers)
    end
  end
end
