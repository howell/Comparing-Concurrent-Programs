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

