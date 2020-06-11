region_manager = RegionManager.spawn

cand_registry = AbstractRegistry.create(CandStruct)

Candidate.spawn("Bernie", 50, 0, cand_registry)
Candidate.spawn("Biden", 25, 0, cand_registry)
Candidate.spawn("Tulsi", 10, 0, cand_registry)
Candidate.spawn("1", 10, 100000, cand_registry)
Candidate.spawn("2", 10, 0, cand_registry)
Candidate.spawn("3", 10, 0, cand_registry)
Candidate.spawn("4", 10, 0, cand_registry)
Candidate.spawn("5", 10, 0, cand_registry)
Candidate.spawn("6", 10, 0, cand_registry)


voter_registry_a = AbstractRegistry.create(VoterStruct)

Voter.spawn("ABC", "A", voter_registry_a, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("DEF", "A", voter_registry_a, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("GHI", "A", voter_registry_a, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("JKL", "A", voter_registry_a, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("MNO", "A", voter_registry_a, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("PQR", "A", voter_registry_a, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("STU", "A", voter_registry_a, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("VWX", "A", voter_registry_a, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("YZ1", "A", voter_registry_a, cand_registry, StupidSort.generate("Biden"))
SleepyVoter.spawn("g", "A", voter_registry_a, cand_registry, StupidSort.generate("Biden"))
VoteLeader.spawn("A", voter_registry_a, cand_registry, region_manager)

voter_registry_b = AbstractRegistry.create(VoterStruct)

Voter.spawn("1", "B", voter_registry_b, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("2", "B", voter_registry_b, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("3", "B", voter_registry_b, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("4", "B", voter_registry_b, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("5", "B", voter_registry_b, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("6", "B", voter_registry_b, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("7", "B", voter_registry_b, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("8", "B", voter_registry_b, cand_registry, StupidSort.generate("Biden"))
Voter.spawn("9", "B", voter_registry_b, cand_registry, StupidSort.generate("Biden"))
VoteLeader.spawn("B", voter_registry_b, cand_registry, region_manager)

voter_registry_c = AbstractRegistry.create(VoterStruct)

Voter.spawn("10", "C", voter_registry_c, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("11", "C", voter_registry_c, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("12", "C", voter_registry_c, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("13", "C", voter_registry_c, cand_registry, StupidSort.generate("Bernie"))
Voter.spawn("14", "C", voter_registry_c, cand_registry, StupidSort.generate("Bernie"))
VoteLeader.spawn("C", voter_registry_c, cand_registry, region_manager)

voter_registry_d = AbstractRegistry.create(VoterStruct)

Voter.spawn("14", "D", voter_registry_d, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("15", "D", voter_registry_d, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("16", "D", voter_registry_d, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("17", "D", voter_registry_d, cand_registry, StupidSort.generate("Tulsi"))
Voter.spawn("18", "D", voter_registry_d, cand_registry, StupidSort.generate("Tulsi"))
VoteLeader.spawn("D", voter_registry_d, cand_registry, region_manager)

voter_registry_e = AbstractRegistry.create(VoterStruct)

Voter.spawn("19", "E", voter_registry_e, cand_registry, StupidSort.generate("6"))
Voter.spawn("20", "E", voter_registry_e, cand_registry, StupidSort.generate("6"))
Voter.spawn("21", "E", voter_registry_e, cand_registry, StupidSort.generate("6"))
Voter.spawn("22", "E", voter_registry_e, cand_registry, StupidSort.generate("6"))
Voter.spawn("23", "E", voter_registry_e, cand_registry, StupidSort.generate("6"))
VoteLeader.spawn("E", voter_registry_e, cand_registry, region_manager)


ref = Process.monitor(region_manager)

receive do
  {:DOWN, ^ref, _, _, _} -> :ok
end

