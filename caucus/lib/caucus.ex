########################### Conversations ###########################
### Structs/Messages ###
# a Name is a String
# a TaxRate is a Number
# a PID is a Process ID
# a Threshold is a Number
# a Region is a String
#
# a Candidate is a %CandStruct{name: Name, tax_rate: TaxRate, pid: PID}
# a Voter is a %VoterStruct{name: Name, pid: PID}
# a Subscription is a {:subscribe, PID}
# a VoteRequest is a {:vote_request, [Setof Candidate], PID}
# a Vote is a {:vote, Name}
# a VoteLeader is a %VoteLeader{pid: PID}
# a Ballot is a {:ballot, [Mapof Name -> Number]}
# the contents of an AbstractRegistry are of the form %AbstractRegistry{values: [Setof X], type: X}

defmodule CandStruct do
  defstruct [:name, :tax_rate, :pid]
end

# Candidates in the Caucus
defmodule Candidate do
  # Notify the Candidate Registry of a new Candidate
  # Name TaxRate Threshold PID -> PID
  def spawn(name, tax_rate, threshold, cand_registry) do
    spawn fn -> 
      send cand_registry, %CandStruct{name: name, tax_rate: tax_rate, pid: self()}
      loop(name, tax_rate, threshold, cand_registry)
    end
  end

  # Listen to messages from other actors and respond accordingly
  # Name TaxRate Threshold PID -> void
  defp loop(name, tax_rate, threshold, cand_registry) do
    receive do
      {:ballot, ballot} -> 
        if ballot[name] < threshold do
          send cand_registry, {:remove, %CandStruct{name: name, tax_rate: tax_rate, pid: self()}}
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
  def create(type, keys \\ false) do
    IO.puts "We exist with type #{inspect type}!"
    spawn fn -> if keys, do: key_loop(type, %{}, %{}), else: loop(type, MapSet.new(), MapSet.new()) end
  end

  def key_loop(type, values, subscribers) do
    receive do
      # Receiving a struct of the module Type, update all subscribers listening to the specified key with new data
      {:publish, key, %^type{} = new_val} ->
        IO.puts "New value: #{inspect new_val} under key #{inspect key} for #{inspect type}!"
        new_values = Map.update(values, key, MapSet.new([new_val]), fn vals -> MapSet.put(vals, new_val) end)
        Enum.each(Map.get(subscribers, key, MapSet.new()), fn s -> send s, the_package(values[key], type) end)
        key_loop(type, new_values, subscribers)
      # Add a new subscriber listening to the specified key and update them with the latest
      {:subscribe, key, pid} ->
        IO.puts "We have a new subscriber! #{inspect pid} listening to key #{inspect key} for #{inspect type}!"
        send pid, the_package(values[key], type)
        key_loop(type, values, Map.update(subscribers, key, MapSet.new([pid]), fn vals -> MapSet.put(vals, pid) end))
      # Send a single-instance message to a process with the most recent snapshot of data at the specified key
      {:msg, key, pid} ->
        IO.puts "Process #{inspect pid} is requesting a message from key #{inspect key}!"
        send pid, the_package(values[key], type)
        key_loop(type, values, subscribers)
      # Unpublish some data specified at the current key
      {:remove, key, val} ->
        IO.puts "Value #{inspect val} is removing itself from the Registry under key #{inspect key}!"
        key_loop(type, Map.update(subscribers, key, MapSet.new, fn vals -> MapSet.delete(vals, val) end), subscribers)
    end
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

# The struct for a Voter declaration
defmodule VoterStruct do
  defstruct [:name, :pid]
end

# Shared behavior between voters
defmodule Voter.Mixin do
  defmacro __using__(_) do
    quote do
      # Initialize a new voter
      # Name Region PID PID ([Setof Candidate] -> [Setof Candidate]) -> PID
      def spawn(name, region, voter_registry, cand_registry, voting_fun) do
        spawn fn -> 
          send voter_registry, {:publish, region, %VoterStruct{name: name, pid: self()}}
          send cand_registry, {:subscribe, self()}
          loop(name, MapSet.new(), voting_fun)
        end
      end

      # Respond to messages sent to a voter
      # Name [Setof Candidate] ([Enumerable Candidate] -> [Listof Candidate]) -> void
      defp loop(name, candidates, voting_fun) do
        receive do
          %AbstractRegistry{values: new_candidates, type: CandStruct} ->
            IO.puts "Voter #{name} has received candidates! #{inspect new_candidates}"
            loop(name, new_candidates, voting_fun)
          {:vote_request, eligible_candidates, vote_leader} ->
            vote(name, candidates, eligible_candidates, vote_leader, voting_fun)
            loop(name, candidates, voting_fun)
        end
      end
    end
  end
end

# A Voter that participates in the Caucus
defmodule Voter do
  use Voter.Mixin
  # Issue a ballot for the voter's preferred candidate
  # Name [Setof Candidate] [Setof Candidate] PID ([Setof Candidate] -> [Setof Candidate]) -> void
  defp vote(name, all_candidates, eligible_candidates, vote_leader, voting_fun) do
    sorted_candidates = voting_fun.(all_candidates)
    %CandStruct{name: voting_for, tax_rate: _, pid: _} = Enum.find(sorted_candidates, fn cand -> MapSet.member?(eligible_candidates, cand) end)
    IO.puts "Voter #{name} is voting for #{voting_for}!"
    send vote_leader, {:vote, name, voting_for}
    loop(name, all_candidates, voting_fun)
  end
end

# A Voter that votes for multiple candidates
defmodule GreedyVoter do
  use Voter.Mixin

  # Issue multiple ballots for the voter's preferred candidates
  # Name [Setof Candidate] [Setof Candidate] PID ([Setof Candidate] -> [Setof Candidate]) -> void
  defp vote(name, all_candidates, eligible_candidates, vote_leader, voting_fun) do
    sorted_candidates = voting_fun.(all_candidates)
    %CandStruct{name: voting_for, tax_rate: _, pid: _} = Enum.find(sorted_candidates, fn cand -> MapSet.member?(eligible_candidates, cand) end)
    %CandStruct{name: second_vote, tax_rate: _, pid: _} = Enum.find(sorted_candidates, fn cand -> MapSet.member?(eligible_candidates, cand) && cand.name != voting_for end)
    IO.puts "Greedy voter #{name} is voting for multiple candidates!"
    send vote_leader, {:vote, name, voting_for}
    send(vote_leader, {:vote, name, if (second_vote) do
                        second_vote
                      else
                        voting_for
                      end})
  end
end

defmodule StubbornVoter do
  use Voter.Mixin

  # Issue a ballot for a voter's preferred candidate, regardless of their status in the race
  # Name [Setof Candidate] [Setof Candidate] PID ([Setof Candidate] -> [Setof Candidate]) -> void
  defp vote(name, all_candidates, _eligible_candidates, vote_leader, voting_fun) do
    sorted_candidates = voting_fun.(all_candidates)
    [%CandStruct{name: voting_for, tax_rate: _, pid: _} | _] = sorted_candidates
    send vote_leader, {:vote, name, voting_for}
  end
end

defmodule SleepyVoter do
  use Voter.Mixin

  # Doesn't issue a ballot
  # 5 arguments -> void
  defp vote(_, _, _, _, _) do
    :noop
  end
end

# The actor that manages voting and elects a winner
defmodule VoteLeader do
  defstruct [:pid]
  # initialize the VoteLeader
  # PID Region PID PID -> PID
  def spawn(region, voter_registry, candidate_registry, region_manager) do
    spawn fn -> 
      send region_manager, %VoteLeader{pid: self()}
      Process.sleep(1000)
      send voter_registry, {:msg, region, self()}
      setup_voting(MapSet.new(), MapSet.new(), candidate_registry, region_manager)
    end
  end

  # Query for any prerequisite data for running a round of voting
  # [Setof Voter] PID -> void
  defp setup_voting(voters, blacklist, candidate_registry, region_manager) do
    send candidate_registry, {:msg, self()}
    prepare_voting(voters, MapSet.new(), blacklist, candidate_registry, region_manager)
  end

  # Gather the information necessary to start voting and issue votes to voters
  # [Setof Voter] [Setof Candidate] PID -> void
  defp prepare_voting(voters, candidates, blacklist, candidate_registry, region_manager) do
    if !(Enum.empty?(voters) || Enum.empty?(candidates)) do
      valid_candidates = MapSet.difference(candidates, blacklist)
      issue_votes(voters, valid_candidates)
      initial_tally = Enum.reduce(valid_candidates, %{}, fn cand, acc -> Map.put(acc, cand.name, 0) end)
      whitelisted_voters = Enum.reduce(voters, %{}, fn voter, acc -> Map.put(acc, voter.name, voter) end)
      Process.send_after self(), :timeout, 1000

      vote_loop(
        voters, 
        valid_candidates, 
        Enum.reduce(valid_candidates, %{}, fn cand, acc -> Map.put(acc, cand.name, cand) end), 
        blacklist, 
        initial_tally, 
        %{}, 
        whitelisted_voters, 
        candidate_registry, 
        region_manager
      )
    else
      receive do
        %AbstractRegistry{values: new_voters, type: VoterStruct} ->
          IO.puts "Vote leader received voters! #{inspect new_voters}"
          prepare_voting(new_voters, candidates, blacklist, candidate_registry, region_manager)
        %AbstractRegistry{values: new_candidates, type: CandStruct} ->
          IO.puts "Vote Leader received candidates! #{inspect new_candidates}"
          prepare_voting(voters, new_candidates, blacklist, candidate_registry, region_manager)
      end
    end
  end

  # Request a vote from all voters with the current eligible candidates
  # [Setof Voter] [Setof Candidate] PID -> void
  defp issue_votes(voters, candidates) do
    IO.puts "Issuing votes!"
    Enum.each(voters, fn %VoterStruct{name: _, pid: pid} -> send pid, {:vote_request, candidates, self()} end)
  end

  # Receive votes from voters and elect a winner if possible
  # [Setof Voter] [Setof Candidate] [Mapof Name -> Candidate] [Mapof Name -> Number] PID -> void
  defp vote_loop(voters, candidates, cand_names, blacklist, tally, voting_record, voter_whitelist, cand_registry, region_manager) do
    receive do
      :timeout ->
        conclude_vote(candidates, cand_names, blacklist, tally, voting_record, voter_whitelist, cand_registry, region_manager)
      {:vote, voter_name, cand_name} -> 
        cond do
          # CASE 1: Stubborn Voter || CASE 2: Greedy Voter
          !Map.has_key?(cand_names, cand_name) || Map.has_key?(voting_record, voter_name) ->
            IO.puts "Voter #{inspect voter_name} has been caught trying to vote for a dropped candidate!"
            update_tally_fun = fn old_val -> 
              if Map.has_key?(cand_names, voting_record[voter_name]) do
                old_val - 1 
              else
                0
              end
            end

            vote_loop(
              MapSet.delete(voters, voter_whitelist[voter_name]),
              candidates,
              cand_names,
              blacklist,
              Map.update(tally, voting_record[voter_name], 0, update_tally_fun),
              Map.delete(voting_record, voter_name),
              Map.delete(voter_whitelist, voter_name),
              cand_registry,
              region_manager
            )
          true ->
            IO.puts "Voter #{inspect voter_name} is voting for candidate #{inspect cand_name}!"
            vote_loop(
              voters,
              candidates,
              cand_names,
              blacklist,
              Map.update(tally, cand_name, 1, &(&1 + 1)),
              Map.put(voting_record, voter_name, cand_name),
              voter_whitelist,
              cand_registry,
              region_manager
            )
        end
    end
  end

  # Determine a winner, or if there isn't one, remove a loser and start next voting loop
  # [Setof Candidate] [Mapof Name -> Candidate] [Setof Candidate] [Mapof Name -> Number] [Mapof Name -> Name] [Setof Voter] PID PID -> void
  defp conclude_vote(candidates, cand_names, blacklist, tally, voting_record, voter_whitelist, cand_registry, region_manager) do
    confirmed_voters = Enum.reduce(voting_record, MapSet.new, fn {voter_name, _}, acc -> MapSet.put(acc, voter_whitelist[voter_name]) end)
    num_votes = Enum.reduce(tally, 0, fn {_, count}, acc -> acc + count end)
    {frontrunner, their_votes} = Enum.max(tally, fn {_, count1}, {_, count2} -> count1 >= count2 end)
    if their_votes > (num_votes / 2) do
      send region_manager, {:results, frontrunner}
    else
      {loser, _} = Enum.min(tally, fn {_, count1}, {_, count2} -> count1 <= count2 end)
      Enum.each(candidates, fn %CandStruct{name: _, tax_rate: _, pid: pid} -> send pid, {:ballot, tally} end)
      IO.puts "Our loser is #{loser}!"

      setup_voting(confirmed_voters, MapSet.put(blacklist, cand_names[loser]), cand_registry, region_manager)
    end
  end
end

# Aggregates the results of many caucuses to determine a winner for a region
defmodule RegionManager do
  def spawn do
    spawn fn -> determine_region(MapSet.new(), %{}) end
  end

  def determine_region(vote_leaders, results) do
    receive do
      %VoteLeader{pid: pid} -> determine_region(MapSet.put(vote_leaders, pid), results)
      {:results, cand_name} ->
        new_results = Map.update(results, cand_name, 1, &(&1 + 1))
        total_results = Enum.reduce(new_results, 0, fn {_, count}, acc -> acc + count end)

        if MapSet.size(vote_leaders) == total_results do
          {victor_name, _} = Enum.max(new_results, fn {_, count1}, {_, count2} -> count1 >= count2 end)
          IO.puts "The winner of the region is: #{inspect victor_name}!"
        else
          determine_region(vote_leaders, new_results)
        end
    end
  end
end

defmodule StupidSort do
  def generate(cand_names) when is_list(cand_names) do
    fn candidates ->
      Enum.reduce(Enum.reverse(cand_names), Enum.sort(candidates), fn cand_name, acc -> new_candidates(cand_name, acc) end)
    end
  end

  def generate(cand_name) do
    fn candidates -> new_candidates(cand_name, Enum.sort(candidates)) end
  end

  defp new_candidates(cand_name, candidates) do
    candidate? = Enum.find(candidates, fn(%CandStruct{name: n, tax_rate: _, pid: _}) -> n == cand_name end)
    if candidate? do
      [candidate? | Enum.reject(candidates, fn(%CandStruct{name: n, tax_rate: _, pid: _}) -> n == cand_name end)]
    else
      candidates
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
