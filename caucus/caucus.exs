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

voter_registry = AbstractRegistry.create(VoterStruct)

# 5 for Tulsi
# 3 for Biden-Tulsi
# 3 for Bernie-Tulsi
Voter.spawn("ABC", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("DEF", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("GHI", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("JKL", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("MNO", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
GreedyVoter.spawn("a", voter_registry, cand_registry, StupidSort.generate(["Biden", "Tulsi"]))
GreedyVoter.spawn("b", voter_registry, cand_registry, StupidSort.generate(["Biden", "Tulsi"]))
GreedyVoter.spawn("c", voter_registry, cand_registry, StupidSort.generate(["Biden", "Tulsi"]))
GreedyVoter.spawn("d", voter_registry, cand_registry, StupidSort.generate(["Bernie", "Tulsi"]))
GreedyVoter.spawn("e", voter_registry, cand_registry, StupidSort.generate(["Bernie", "Tulsi"]))
GreedyVoter.spawn("f", voter_registry, cand_registry, StupidSort.generate(["Bernie", "Tulsi"]))
Voter.spawn("j", voter_registry, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("k", voter_registry, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("l", voter_registry, cand_registry, StupidSort.generate("Bernie"))



pid = VoteLeader.spawn(voter_registry, cand_registry)
ref = Process.monitor(pid)

receive do
  {:DOWN, ^ref, _, _, _} -> :ok
end

