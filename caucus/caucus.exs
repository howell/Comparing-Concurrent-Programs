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
Voter.spawn("JKL", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("MNO", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("PQR", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("STU", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("VWX", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("YZ1", voter_registry, cand_registry, StupidSort.generate("Biden"))

StubbornVoter.spawn("1", voter_registry, cand_registry, StupidSort.generate("1"))
StubbornVoter.spawn("2", voter_registry, cand_registry, StupidSort.generate("2"))
StubbornVoter.spawn("3", voter_registry, cand_registry, StupidSort.generate("1"))
StubbornVoter.spawn("4", voter_registry, cand_registry, StupidSort.generate("2"))
StubbornVoter.spawn("5", voter_registry, cand_registry, StupidSort.generate("1"))
StubbornVoter.spawn("6", voter_registry, cand_registry, StupidSort.generate("3"))



pid = VoteLeader.spawn(voter_registry, cand_registry)
ref = Process.monitor(pid)

receive do
  {:DOWN, ^ref, _, _, _} -> :ok
end

