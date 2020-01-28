module Schloss (
    Command,
    Event,
    Actor,
) where

import Data.UUID

type family Command = result
type family Event = result
type family Actor = result

-- data Command = Command {
--     command_id :: UUID,
--     command_payload :: *
-- }
--
-- data Event = Event {
--     event_id :: UUID,
--     event_payload :: *
-- }
--
-- data Actor = Actor {
--     actor_emits :: [Command],
--     actor_reacts_to :: [Event]
-- }
