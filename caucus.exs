defmodule Candidate do
  defstruct [:name, :tax_rate]

  def spawn(name, tax_rate, cand_registry) do
    spawn fn -> send cand_registry, %Candidate{name: name, tax_rate: tax_rate} end
  end
end

defmodule CandidateRegistry do
  defstruct [:candidates]

  def spawn do
    spawn fn -> loop(MapSet.new(), MapSet.new()) end 
  end

  defp loop(candidates, subscribers) do
    receive do
      %Candidate{name: n, tax_rate: tr} -> 
        IO.puts n
        new_candidates = MapSet.put(candidates, %Candidate{name: n, tax_rate: tr})
        Enum.each(subscribers, fn s -> send s, %CandidateRegistry{candidates: new_candidates} end)
        loop(new_candidates, subscribers)
      {:subscribe, pid} ->
        IO.puts "Subscriber #{inspect pid} has subscribed!"
        send pid, %CandidateRegistry{candidates: candidates}
        loop(candidates, MapSet.put(subscribers, pid))
    end
  end
end

defmodule Voter do
  defstruct [:name, :pid]

  def spawn(name, voter_registry, cand_registry, voting_fun) do
    spawn fn -> 
      send voter_registry, %Voter{name: name, pid: self()}
      send cand_registry, {:subscribe, self()}
      loop(MapSet.new(), voting_fun)
    end
  end

  defp loop(candidates, voting_fun) do
    receive do
      %CandidateRegistry{candidates: new_candidates} -> 
        IO.puts "Candidates received! #{inspect new_candidates}"
        loop(new_candidates, voting_fun)
      {:vote_request, eligible_candidates, vote_leader} ->
        sorted_candidates = voting_fun.(candidates)
        IO.puts "A voter is voting with #{inspect(sorted_candidates)}"
        %Candidate{name: name, tax_rate: _} = Enum.find(sorted_candidates, fn cand -> MapSet.member?(eligible_candidates, cand) end)
        send vote_leader, {:vote, name}
        loop(candidates, voting_fun)
    end
  end
end

defmodule VoterRegistry do
  defstruct [:voters]

  def spawn do
    spawn fn -> loop(MapSet.new(), MapSet.new()) end
  end

  defp loop(voters, subscribers) do
    receive do
      %Voter{name: name, pid: voter_pid} -> 
        IO.puts name
        loop(MapSet.put(voters, %Voter{name: name, pid: voter_pid}), subscribers)
      {:subscribe, pid} ->
        IO.puts "New subscriber to the voter registry: #{inspect pid}!"
        send pid, %VoterRegistry{voters: voters}
        loop(voters, MapSet.put(subscribers, pid))
    end
  end
end

defmodule VoteLeader do
  def spawn(voter_registry, candidate_registry) do
    spawn fn -> 
      Process.sleep(1000)
      send voter_registry, {:subscribe, self()}
      send candidate_registry, {:subscribe, self()}
      setup_voting(MapSet.new(), MapSet.new())
    end
  end

  defp setup_voting(voters, candidates) do
    if !(Enum.empty?(voters) || Enum.empty?(candidates)) do
      IO.puts "WWOWOWOWO"
      IO.puts inspect(candidates)
      issue_votes(voters, candidates)
      vote_loop(voters, candidates, %{})
    else
      receive do
        %VoterRegistry{voters: new_voters} ->
          IO.puts "Vote leader received voters! #{inspect new_voters}"
          setup_voting(new_voters, candidates)
        %CandidateRegistry{candidates: new_candidates} ->
          IO.puts "Vote Leader received candidates! #{inspect new_candidates}"
          setup_voting(voters, new_candidates)
      end
    end
  end

  defp issue_votes(voters, candidates) do
    IO.puts "Issuing votes!"
    Enum.each(voters, fn %Voter{name: _, pid: pid} -> send pid, {:vote_request, candidates, self()} end)
  end

  defp vote_loop(voters, candidates, tally) do
    num_votes = Enum.reduce(tally, 0, fn {_, count}, acc -> acc + count end)
    if Enum.count(voters) == num_votes do
      {frontrunner, their_votes} = Enum.max(tally, fn {_, count1}, {_, count2} -> count1 >= count2 end)
      if their_votes > (num_votes / 2) do
        IO.puts "And the winner is: #{frontrunner}!"
      else
        {loser, _} = Enum.min(tally, fn {_, count1}, {_, count2} -> count1 <= count2 end)
        IO.puts "Our loser is #{loser}!"
        setup_voting(voters, MapSet.new(Enum.reject(candidates, fn %Candidate{name: n, tax_rate: _} -> n == loser end)))
      end
    else
      receive do
        {:vote, cand_name} -> vote_loop(voters, candidates, Map.update(tally, cand_name, 1, &(&1 + 1)))
      end
    end
  end
end

defmodule StupidSort do
  def generate(cand_name) do
    fn candidates ->
      candidate? = Enum.find(candidates, fn(%Candidate{name: n, tax_rate: tr}) -> n == cand_name end)

      if candidate? do
        # no idea if this is correct
        [candidate? | Enum.reject(candidates, fn(%Candidate{name: n, tax_rate: _}) -> n == cand_name end)]
      else
        candidates
      end
    end
  end
end

defmodule MockSubscriber do
  def mock_spawn(pubsub) do
    spawn fn -> 
      send pubsub, {:subscribe, self()}
      loop()
    end
  end

  defp loop do
    receive do
      any -> IO.puts inspect(any)
    end
  end
end

cand_registry = CandidateRegistry.spawn

Candidate.spawn("Bernie", 50, cand_registry)
Candidate.spawn("Biden", 25, cand_registry)
Candidate.spawn("Tulsi", 10, cand_registry)

voter_registry = VoterRegistry.spawn

Voter.spawn("ABC", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("DEF", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("GHI", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("JKL", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("123", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("124", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("125", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("118", voter_registry, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("108", voter_registry, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("107", voter_registry, cand_registry, StupidSort.generate("Bernie"))

VoteLeader.spawn(voter_registry, cand_registry)

# Question: when does the program decide to sleep? What's the order of events that causes the program to terminate before the voters receive their msg?

Process.sleep(10000)

