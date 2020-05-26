### Syndicate v. The World
1. The dataspace eliminates the need for configuration (e.g. don't need to wait for other actors/threads to send certain information to a manager, it just exists in the dataspace)
2. Facets abstract away control flow
3. Syndicate makes it easier to manage receiving the same type of message across different states of the same actor

### Syndicate v. Channels
1. Synchronization occurs via channel-passing, so don't need to color everything w/region in the channels version

### Actors v. Channels
1. Don't have a dedicated channel for each conversation, just have a mailbox
