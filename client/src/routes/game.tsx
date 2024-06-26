import React, { useState } from "react";
import moon from "../assets/images/moon.svg";
interface Role {
  name: string;
  desc: string;
  avatar: string;
  mafia: boolean;
}
interface Player {
  name: string;
  id: number;
  role: Role;
}

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
const PlayerCard: React.FC<{ player: Player }> = ({ player }) => {
  return (
    <button className="flex hover:bg-violet p-2 rounded-lg justify-between py-2">
      <div className="flex items-center gap-2 ">
        <img
          src={player.role.avatar}
          className="object-cover  w-12 h-12 rounded-md shrink-0 dark:border-none"
          loading="lazy"
          alt="nejaa badr"
        />
        <div>
          <p
            className={`text-sm  ${
              player.role.mafia ? "text-red" : "text-mint"
            }`}
          >
            {player.role.name}
          </p>
          <p className="text-gray-700 dark:text-gray-300 hover:text-primary-800">
            {player.name}
          </p>
        </div>
      </div>
    </button>
  );
};
const Game: React.FC = () => {
  const [number, setNumber] = useState(1);
  return (
    <div className="bg-midnight  space-y-4 flex flex-col p-4 items-center max-w-2xl mx-auto">
      <div
        className="bg-stormy flex justify-evenly font-bold w-1/6 py-2 text-2xl rounded-xl"
        onClick={() => setNumber(number + 1)}
      >
        <img src={moon} className=" w-8 h-8" />
        <div className="text-2xl">{number}</div>
      </div>
      <div className="flex flex-row justify-around w-full">
        <div className="flex flex-col">
          {players
            .filter((player) => player.role.name != "Villager")
            .map((player) => (
              <PlayerCard player={player} />
            ))}
        </div>
        <div className="flex flex-col">
          {players.map((player) => (
            <PlayerCard player={player} />
          ))}
        </div>
      </div>
    </div>
  );
};

export default Game;
