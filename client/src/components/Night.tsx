import React, { useState } from "react";
import moon from "../assets/images/moon.svg";
import sun from "../assets/images/sun.svg";
import type { Player, Event } from "../lib/types";
import PlayerCard from "./PlayerCard";

const players: Player[] = [
  {
    id: 1,
    name: "player 1",
    role: {
      name: "Godfather",
      desc: "Role: The Godfather is the leader of the Mafia. They appear as an innocent townsperson when investigated by the Detective. Abilities: Innocent Appearance: When investigated by the Detective, the Godfather will show up as a Villager, making them difficult to identify as Mafia. Authority: The Godfather may have the final say in choosing the Mafia's target for elimination each night. \nObjective: Guide the Mafia to victory by coordinating attacks, deceiving the town, and avoiding detection. The Godfather must use their unique ability to mislead the town and protect their fellow Mafia members.",
      avatar: "http://localhost:5173/src/assets/images/godfather.svg",
      mafia: true,
    },
  },
  {
    id: 2,
    name: "player 2",
    role: {
      name: "Mafia",
      desc: "Role: The Mafia members work together to eliminate the Villagers and avoid detection. Each night, they choose one player to eliminate. Objective: Deceive the town and eliminate all non-Mafia players without getting caught.",
      avatar: "http://localhost:5173/src/assets/images/mafia.svg",
      mafia: true,
    },
  },
  {
    id: 3,
    name: "player 3",
    role: {
      name: "Detective",
      desc: "Role: Each night, the Detective can investigate one player to determine if they are a member of the Mafia or not. Objective: Use their investigations to identify and help eliminate the Mafia members, protecting the Villagers.",
      avatar: "http://localhost:5173/src/assets/images/godfather.svg",
      mafia: false,
    },
  },
  {
    id: 4,
    name: "player 4",
    role: {
      name: "Leon",
      desc: "Role: Leon could be a special role with unique abilities depending on the version of the game being played. For instance, Leon might have a single-use ability to revive a player, protect themselves, or have another unique skill. Objective: Depending on the specific rules, Leon's objective could vary but usually aligns with either the town's or the Mafia's goal, or they might have a unique win condition.",
      avatar: "http://localhost:5173/src/assets/images/leon.svg",
      mafia: false,
    },
  },
  {
    id: 5,
    name: "player 5",
    role: {
      name: "Doctor",
      desc: "Role: Each night, the Doctor can choose one player to protect from being eliminated by the Mafia. \nObjective: Protect key town players (such as the Detective) and help the town survive long enough to eliminate the Mafia.",
      avatar: "http://localhost:5173/src/assets/images/doctor.svg",
      mafia: false,
    },
  },
  {
    id: 6,
    name: "player 6",
    role: {
      name: "Villager",
      desc: "Role: Regular town members with no special abilities. They vote during the day to eliminate a player they suspect to be Mafia. Objective: Use discussion, logic, and social deduction to identify and votme out the Mafia members.",
      avatar: "http://localhost:5173/src/assets/images/villager.svg",
      mafia: false,
    },
  },
  {
    id: 7,
    name: "player 7",
    role: {
      name: "Villager",
      desc: "Role: Regular town members with no special abilities. They vote during the day to eliminate a player they suspect to be Mafia. Objective: Use discussion, logic, and social deduction to identify and votme out the Mafia members.",
      avatar: "http://localhost:5173/src/assets/images/villager.svg",
      mafia: false,
    },
  },
];
const events: Event[] = [
  { actionType: "kill", subject: "player 1", target: "player 6" },
];
interface NightProps {
  handlePhaseChange: () => void;
}
const Night: React.FC<NightProps> = ({ handlePhaseChange }) => {
  const [number, setNumber] = useState(1);
  return (
    <div className="bg-midnight  space-y-4 flex flex-col p-4 items-center max-w-2xl mx-auto">
      <div className="flex flex-row justify-between w-full ">
        <div
          className="bg-stormy flex justify-evenly font-bold w-1/6 py-2 text-2xl rounded-xl"
          onClick={() => setNumber(number + 1)}
        >
          <img src={moon} className=" w-8 h-8" />
          <div className="text-2xl">{number}</div>
        </div>
        <button
          className="bg-peach flex justify-evenly space-x-1 p-1 items-center font-bold py-2 text-2xl rounded-xl"
          onClick={handlePhaseChange}
        >
          <img src={sun} className=" w-8 h-8" />
          <p className="text-sm">Next Day</p>
        </button>
      </div>
      <div className="flex flex-row justify-between w-full">
        <div className="flex flex-col space-y-2">
          <h5 className="text-xl text-lavender text-left">Player with Action </h5>

          {players
            .filter((player) => player.role.name != "Villager")
            .map((player, index) => (
              <div className="flex flex-row items-center space-x-2">
                <PlayerCard active = {index >= events.length  }key={player.id} player={player} />
                {events
                  .filter((e) => e.subject === player.name)
                  .map((event) => (
                    <div className="flex flex-row space-x-1">
                      <p className="text-sm rounded-lg bg-pink py-1 px-2  bg- text-black ">{event.actionType}</p>
                      <p className="text-sm p-1 text-white ">{event.target}</p>
                    </div>
                  ))}
              </div>
            ))}
        </div>
        <div className="flex flex-col space-y-2">
          <h5 className="text-xl text-lavender text-right">Targeted Player</h5>
          {players.map((player) => (
            <PlayerCard key={player.id} player={player} />
          ))}
        </div>
      </div>
    </div>
  );
};

export default Night;
