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
          send cand_registry, {:remove, %Candidate{name: name, tax_rate: tax_rate, pid: self()}}
        else
          loop(name, tax_rate, threshold, cand_registry)
        end
    end
  end
end

# A pub/sub server for some type of Struct
defmodule AbstractRegistry do
  defstruct [:values, :type]

  # initialize a new AbstractRegistry
  def create(type) do
    IO.puts "We exist with type #{inspect type}!"
    spawn fn -> loop(type, MapSet.new(), MapSet.new()) end
  end

  def loop(type, values, subscribers) do
    receive do
      # Receiving a struct of the module Type, update all current subscribers with new data
      %^type{} = new_val ->
        IO.puts "New value: #{inspect new_val} for #{inspect type}!"
        new_values = MapSet.put(values, new_val)
        Enum.each(subscribers, fn s -> send s, the_package(values, type) end)
        loop(type, new_values, subscribers)
      # Add a new subscriber to the list and update them with the latest
      {:subscribe, pid} -> 
        IO.puts "We have a new subscriber! #{inspect pid} for #{inspect type}!"
        send pid, the_package(values, type)
        loop(type, values, MapSet.put(subscribers, pid))
      # Send a single-instance message to a process of the most recent snapshot of data
      {:msg, pid} ->
        IO.puts "Process #{inspect pid} is requesting a message!"
        send pid, the_package(values, type)
        loop(type, values, subscribers)
      # Remove a piece of data from the published data
      {:remove, val} ->
        IO.puts "Value #{inspect val} is removing itself from the Registry!"
        loop(type, MapSet.delete(values, val), subscribers)
    end
  end

  def the_package(values, type) do
    %AbstractRegistry{values: values, type: type}
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
      %AbstractRegistry{values: new_candidates, type: Candidate} -> 
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

# The actor that manages voting and elects a winner
defmodule VoteLeader do
  # initialize the VoteLeader
  # PID PID -> PID
  def spawn(voter_registry, candidate_registry) do
    spawn fn -> 
      Process.sleep(1000)
      send voter_registry, {:subscribe, self()}
      setup_voting(MapSet.new(), MapSet.new(), candidate_registry)
    end
  end

  # Query for any prerequisite data for running a round of voting
  # [Setof Voter] PID -> void
  defp setup_voting(voters, blacklist, candidate_registry) do
    send candidate_registry, {:msg, self()}
    prepare_voting(voters, MapSet.new(), blacklist, candidate_registry)
  end

  # Gather the information necessary to start voting and issue votes to voters
  # [Setof Voter] [Setof Candidate] PID -> void
  defp prepare_voting(voters, candidates, blacklist, candidate_registry) do
    if !(Enum.empty?(voters) || Enum.empty?(candidates)) do
      valid_candidates = MapSet.difference(candidates, blacklist)
      issue_votes(voters, valid_candidates)
      vote_loop(voters, valid_candidates, Enum.reduce(valid_candidates, %{}, fn cand, acc -> Map.put(acc, cand.name, cand) end), blacklist, %{}, candidate_registry)
    else
      receive do
        %AbstractRegistry{values: new_voters, type: Voter} ->
          IO.puts "Vote leader received voters! #{inspect new_voters}"
          prepare_voting(new_voters, candidates, blacklist, candidate_registry)
        %AbstractRegistry{values: new_candidates, type: Candidate} ->
          IO.puts "Vote Leader received candidates! #{inspect new_candidates}"
          prepare_voting(voters, new_candidates, blacklist, candidate_registry)
      end
    end
  end

  # Request a vote from all voters with the current eligible candidates
  # [Setof Voter] [Setof Candidate] PID -> void
  defp issue_votes(voters, candidates) do
    IO.puts "Issuing votes!"
    Enum.each(voters, fn %Voter{name: _, pid: pid} -> send pid, {:vote_request, candidates, self()} end)
  end

  # Receive votes from voters and elect a winner if possible
  # [Setof Voter] [Setof Candidate] [Mapof Name -> Candidate] [Mapof Name -> Number] PID -> void
  defp vote_loop(voters, candidates, cand_names, blacklist, tally, cand_registry) do
    num_votes = Enum.reduce(tally, 0, fn {_, count}, acc -> acc + count end)
    if Enum.count(voters) == num_votes do
      IO.puts "The final tally is: #{inspect tally}!"
      {frontrunner, their_votes} = Enum.max(tally, fn {_, count1}, {_, count2} -> count1 >= count2 end)
      if their_votes > (num_votes / 2) do
        IO.puts "And the winner is: #{frontrunner}!"
      else
        {loser, _} = Enum.min(tally, fn {_, count1}, {_, count2} -> count1 <= count2 end)
        Enum.each(candidates, fn %Candidate{name: _, tax_rate: _, pid: pid} -> send pid, {:ballot, tally} end)
        IO.puts "Our loser is #{loser}!"

        setup_voting(voters, MapSet.put(blacklist, cand_names[loser]), cand_registry)
      end
    else
      receive do
        {:vote, cand_name} -> vote_loop(voters, candidates, cand_names, blacklist, Map.update(tally, cand_name, 1, &(&1 + 1)), cand_registry)
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

cand_registry = AbstractRegistry.create(Candidate)

Candidate.spawn("Bernie", 50, 0, cand_registry)
Candidate.spawn("Biden", 25, 0, cand_registry)
Candidate.spawn("Tulsi", 10, 0, cand_registry)
Candidate.spawn("1", 10, 100000, cand_registry)
Candidate.spawn("2", 10, 0, cand_registry)
Candidate.spawn("3", 10, 0, cand_registry)
Candidate.spawn("4", 10, 0, cand_registry)
Candidate.spawn("5", 10, 0, cand_registry)
Candidate.spawn("6", 10, 0, cand_registry)

voter_registry = AbstractRegistry.create(Voter)

Voter.spawn("ABC", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("DEF", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("GHI", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("JKL", voter_registry, cand_registry, StupidSort.generate("Tulsi"))

Voter.spawn("123", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("124", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("125", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("CBA", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("FED", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("IHG", voter_registry, cand_registry, StupidSort.generate("Biden"))

Voter.spawn("118", voter_registry, cand_registry, StupidSort.generate("1"))
Voter.spawn("108", voter_registry, cand_registry, StupidSort.generate("2"))
Voter.spawn("107", voter_registry, cand_registry, StupidSort.generate("3"))
Voter.spawn("151", voter_registry, cand_registry, StupidSort.generate("4"))
Voter.spawn("169", voter_registry, cand_registry, StupidSort.generate("5"))
Voter.spawn("186", voter_registry, cand_registry, StupidSort.generate("6"))
Voter.spawn("207", voter_registry, cand_registry, StupidSort.generate("2"))
Voter.spawn("230", voter_registry, cand_registry, StupidSort.generate("1"))
Voter.spawn("A", voter_registry, cand_registry, StupidSort.generate("2"))
Voter.spawn("B", voter_registry, cand_registry, StupidSort.generate("2"))
Voter.spawn("C", voter_registry, cand_registry, StupidSort.generate("2"))
Voter.spawn("D", voter_registry, cand_registry, StupidSort.generate("2"))
Voter.spawn("E", voter_registry, cand_registry, StupidSort.generate("2"))

pid = VoteLeader.spawn(voter_registry, cand_registry)
ref = Process.monitor(pid)

receive do
  {:DOWN, ^ref, _, _, _} -> :ok
end

