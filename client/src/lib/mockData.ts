import type { PlayerWithRole } from "../lib/types";
export const players: PlayerWithRole[] = [
  {
    id: '1',
    name: "player 1",
    alive: true,
    role: {
      name: "Godfather",
      desc: "Role: The Godfather is the leader of the Mafia. They appear as an innocent townsperson when investigated by the Detective. Abilities: Innocent Appearance: When investigated by the Detective, the Godfather will show up as a Villager, making them difficult to identify as Mafia. Authority: The Godfather may have the final say in choosing the Mafia's target for elimination each night. \nObjective: Guide the Mafia to victory by coordinating attacks, deceiving the town, and avoiding detection. The Godfather must use their unique ability to mislead the town and protect their fellow Mafia members.",
      avatar: "http://res.cloudinary.com/m3hransh/image/upload/v1721553956/mehran/mafia/godfather.svg",
      mafia: true,
    },
  },
  {
    id: '2',
    name: "player 2",
    alive: true,
    role: {
      name: "Mafia",
      desc: "Role: The Mafia members work together to eliminate the Villagers and avoid detection. Each night, they choose one player to eliminate. Objective: Deceive the town and eliminate all non-Mafia players without getting caught.",
      avatar: "http://res.cloudinary.com/m3hransh/image/upload/v1721553956/mehran/mafia/mafia.svg",
      mafia: true,
    },
  },
  {
    id: '3',
    name: "player 3",
    alive: true,
    role: {
      name: "Detective",
      desc: "Role: Each night, the Detective can investigate one player to determine if they are a member of the Mafia or not. Objective: Use their investigations to identify and help eliminate the Mafia members, protecting the Villagers.",
      avatar: "http://res.cloudinary.com/m3hransh/image/upload/v1721553956/mehran/mafia/detective.svg",
      mafia: false,
    },
  },
  {
    id: '4',
    name: "player 4",
    alive: true,
    role: {
      name: "Leon",
      desc: "Role: Leon could be a special role with unique abilities depending on the version of the game being played. For instance, Leon might have a single-use ability to revive a player, protect themselves, or have another unique skill. Objective: Depending on the specific rules, Leon's objective could vary but usually aligns with either the town's or the Mafia's goal, or they might have a unique win condition.",
      avatar: "http://res.cloudinary.com/m3hransh/image/upload/v1721553956/mehran/mafia/leon.svg",
      mafia: false,
    },
  },
  {
    id: '5',
    name: "player 5",
    alive: true,
    role: {
      name: "Doctor",
      desc: "Role: Each night, the Doctor can choose one player to protect from being eliminated by the Mafia. \nObjective: Protect key town players (such as the Detective) and help the town survive long enough to eliminate the Mafia.",
      avatar: "http://res.cloudinary.com/m3hransh/image/upload/v1721557675/mehran/mafia/doctor.svg",
      mafia: false,
    },
  },
  {
    id: '6',
    name: "player 6",
    alive: false,
    role: {
      name: "Villager",
      desc: "Role: Regular town members with no special abilities. They vote during the day to eliminate a player they suspect to be Mafia. Objective: Use discussion, logic, and social deduction to identify and votme out the Mafia members.",
      avatar: "http://res.cloudinary.com/m3hransh/image/upload/v1721553956/mehran/mafia/villager.svg",
      mafia: false,
    },
  },
  {
    id: '7',
    name: "player 7",
    alive: false,
    role: {
      name: "Villager",
      desc: "Role: Regular town members with no special abilities. They vote during the day to eliminate a player they suspect to be Mafia. Objective: Use discussion, logic, and social deduction to identify and votme out the Mafia members.",
      avatar: "http://res.cloudinary.com/m3hransh/image/upload/v1721553956/mehran/mafia/villager.svg",
      mafia: false,
    },
  },
];
