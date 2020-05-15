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
  defstruct [:name]

  def spawn(name, voter_registry, cand_registry) do
    spawn fn -> 
      send voter_registry, %Voter{name: name} 
      send cand_registry, {:subscribe, self()}
      loop(MapSet.new())
    end
  end

  defp loop(_candidates) do
    receive do
      %CandidateRegistry{candidates: new_candidates} -> 
        IO.puts "Candidates received! #{inspect new_candidates}"
        loop(new_candidates)
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
      %Voter{name: name} -> 
        IO.puts name
        loop(MapSet.put(voters, %Voter{name: name}), subscribers)
      {:subscribe, pid} ->
        IO.puts "New subscriber to the voter registry: #{inspect pid}!"
        send pid, %VoterRegistry{voters: voters}
        loop(voters, MapSet.put(subscribers, pid))
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

defmodule VoteLeader do
  def spawn do
  end

  def loop do
    # 1. wait for candidate and voter information
    # 2. ask all voters to vote
    # 3. all voters vote
    # 4. if winner, announce so and exit
    # 5. else, eliminate worst-performing contender and continue
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

cand_registry = CandidateRegistry.spawn

Candidate.spawn("Bernie", 50, cand_registry)
Candidate.spawn("Biden", 25, cand_registry)
Candidate.spawn("Tulsi", 10, cand_registry)

voter_registry = VoterRegistry.spawn

Voter.spawn("ABC", voter_registry, cand_registry)
Voter.spawn("DEF", voter_registry, cand_registry)
Voter.spawn("GHI", voter_registry, cand_registry)
Voter.spawn("JKL", voter_registry, cand_registry)

MockSubscriber.mock_spawn(voter_registry)

# Question: when does the program decide to sleep? What's the order of events that causes the program to terminate before the voters receive their msg?

Process.sleep(10000)


# TODO
# 1. subscribers for the voter registry
# 2. vote leader
# 3. voting
# 4. messages as opposed to pubsub to prevent deadlocks
