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

voter_registry = AbstractRegistry.create(VoterStruct, true)

Voter.spawn("ABC", "A", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("DEF", "A", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("GHI", "A", voter_registry, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("JKL", "A", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("MNO", "A", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("PQR", "A", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("STU", "A", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("VWX", "A", voter_registry, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("YZ1", "A", voter_registry, cand_registry, StupidSort.generate("Biden"))

SleepyVoter.spawn("g", "A", voter_registry, cand_registry, StupidSort.generate("Biden"))

region_manager = RegionManager.spawn

VoteLeader.spawn("A", voter_registry, cand_registry, region_manager)
ref = Process.monitor(region_manager)

receive do
  {:DOWN, ^ref, _, _, _} -> :ok
end

