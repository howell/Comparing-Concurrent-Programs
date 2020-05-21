########################### Conversations ###########################
### Structs/Messages ###
# a Name is a String
# a TaxRate is a Number
# a PID is a Process ID
# a Threshold is a Number
#
#
# 1. a Candidate is a %Candidate{name: Name, tax_rate: TaxRate, pid: PID}
# 2. a CandidateRegistry is a %CandidateRegistry{candidates: [Setof Candidate]}
# 3. a Voter is a %Voter{name: Name, pid: PID}
# 4. a VoterRegistry is a %VoterRegistry{voters: [Setof Voter]}
# 5. a Subscription is a {:subscribe, PID}
# 6. a VoteRequest is a {:vote_request, [Setof Candidate], PID}
# 7. a Vote is a {:vote, Name}

###### TODOS ######
# 1. stubborn candidates
# 2. candidates that drop out
# 3. late joining voter
# 4. greedy voter
# 5. stubborn voter
# 6. early leaving voter
# 7. unresponsive voter
# 8. crashing voter?
# 9. unregistered voter?

# Candidates in the Caucus
defmodule Candidate do
  defstruct [:name, :tax_rate, :pid]

  # Notify the Candidate Registry of a new Candidate
  # Name TaxRate Threshold PID -> PID
  def spawn(name, tax_rate, threshold, cand_registry) do
    spawn fn -> 
      send cand_registry, %Candidate{name: name, tax_rate: tax_rate, pid: self()}
      loop(name, tax_rate, threshold, cand_registry)
    end
  end

  # Listen to messages from other actors and respond accordingly
  # Name TaxRate Threshold PID -> void
  defp loop(name, tax_rate, threshold, cand_registry) do
    receive do
      {:ballot, ballot} -> 
        if ballot[name] < threshold do
          send cand_registry, {:drop_out, %Candidate{name: name, tax_rate: tax_rate, pid: self()}}
        else
          loop(name, tax_rate, threshold, cand_registry)
        end
    end
  end
end

# The warehouse of all Candidates: a PubSub actor
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

# A Voter that participates in the Caucus
defmodule Voter do
  defstruct [:name, :pid]

  # Initialize a new voter
  # Name PID PID ([Setof Candidate] -> [Setof Candidate]) -> PID
  def spawn(name, voter_registry, cand_registry, voting_fun) do
    spawn fn -> 
      send voter_registry, %Voter{name: name, pid: self()}
      send cand_registry, {:subscribe, self()}
      loop(name, MapSet.new(), voting_fun)
    end
  end

  # Listen to new messages and respond accordingly
  # Name [Setof Candidate] ([Setof Candidate] -> [Setof Candidate]) -> void
  defp loop(name, candidates, voting_fun) do
    receive do
      %CandidateRegistry{candidates: new_candidates} -> 
        IO.puts "Voter #{name} has received candidates! #{inspect new_candidates}"
        loop(name, new_candidates, voting_fun)
      {:vote_request, eligible_candidates, vote_leader} ->
        sorted_candidates = voting_fun.(candidates)
        %Candidate{name: cand_name, tax_rate: _, pid: _} = Enum.find(sorted_candidates, fn cand -> MapSet.member?(eligible_candidates, cand) end)
        IO.puts "Voter #{name} is voting for #{cand_name}!"
        send vote_leader, {:vote, cand_name}
        loop(name, candidates, voting_fun)
    end
  end
end

# The warehouse of all voters: a PubSub actor
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

# The actor that manages voting and elects a winner
defmodule VoteLeader do
  # initialize the VoteLeader
  # PID PID -> PID
  def spawn(voter_registry, candidate_registry) do
    spawn fn -> 
      Process.sleep(1000)
      send voter_registry, {:subscribe, self()}
      setup_voting(MapSet.new(), candidate_registry)
    end
  end

  # Query for any prerequisite data for running a round of voting
  # [Setof Voter] PID -> void
  defp setup_voting(voters, candidate_registry) do
    send candidate_registry, {:msg, self()}
    prepare_voting(voters, MapSet.new(), candidate_registry)
  end

  # Gather the information necessary to start voting and issue votes to voters
  # [Setof Voter] [Setof Candidate] PID -> void
  defp prepare_voting(voters, candidates, candidate_registry) do
    if !(Enum.empty?(voters) || Enum.empty?(candidates)) do
      issue_votes(voters, candidates)
      vote_loop(voters, candidates, Enum.reduce(candidates, %{}, fn cand, acc -> Map.put(acc, cand.name, cand) end), %{}, candidate_registry)
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

  # Request a vote from all voters with the current eligible candidates
  # [Setof Voter] [Setof Candidate] PID -> void
  defp issue_votes(voters, candidates) do
    IO.puts "Issuing votes!"
    Enum.each(voters, fn %Voter{name: _, pid: pid} -> send pid, {:vote_request, candidates, self()} end)
  end

  # TODO
  # 1. implement a blacklist (or whatever mechanism) for dropping candidates and taking intersection
  # 2. loop and correct args for `setup_voting` calls

  # Receive votes from voters and elect a winner if possible
  # [Setof Voter] [Setof Candidate] [Mapof Name -> Candidate] [Mapof Name -> Number] PID -> void
  defp vote_loop(voters, candidates, cand_names, tally, cand_registry) do
    num_votes = Enum.reduce(tally, 0, fn {_, count}, acc -> acc + count end)
    if Enum.count(voters) == num_votes do
      {frontrunner, their_votes} = Enum.max(tally, fn {_, count1}, {_, count2} -> count1 >= count2 end)
      if their_votes > (num_votes / 2) do
        IO.puts "And the winner is: #{frontrunner}!"
      else
        {loser, _} = Enum.min(tally, fn {_, count1}, {_, count2} -> count1 <= count2 end)
        Enum.each(candidates, fn %Candidate{name: _, tax_rate: _, pid: pid} -> send pid, tally end)
        IO.puts "Our loser is #{loser}!"

        setup_voting(voters, MapSet.delete(candidates, cand_names[loser]))
      end
    else
      receive do
        {:vote, cand_name} -> vote_loop(voters, candidates, cand_names, Map.update(tally, cand_name, 1, &(&1 + 1)))
      end
    end
  end
end

defmodule StupidSort do
  def generate(cand_name) do
    fn candidates ->
      candidate? = Enum.find(candidates, fn(%Candidate{name: n, tax_rate: _, pid: _}) -> n == cand_name end)

      if candidate? do
        [candidate? | Enum.reject(candidates, fn(%Candidate{name: n, tax_rate: _, pid: _}) -> n == cand_name end)]
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

pid = VoteLeader.spawn(voter_registry, cand_registry)
ref = Process.monitor(pid)

receive do
  {:DOWN, ^ref, _, _, _} -> :ok
end

