import type { Player, Role } from './types';
import { produce } from "immer"

export function distributeRoles(players: Player[], roles: { [key: string]: number }): Player[] {
  const rolesToDistribute = Object.keys(roles).reduce<string[]>((acc, key) => {
    for (let i = 0; i < roles[key]; i++) {
      acc.push(key);
    }
    return acc;
  }, []);
  const shuffledRoles = shuffle(rolesToDistribute)
  const playersWithRoles = produce(players, draft => {
    draft.forEach((player, index) => {
      player.role = shuffledRoles[index];
    });
  })
  return playersWithRoles;
}

export function shuffle(array: string[]): string[] {
  const shuffled = [...array];
  for (let i = shuffled.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [shuffled[i], shuffled[j]] = [shuffled[j], shuffled[i]];
  }
  return shuffled;
}

export function findRoleByName(roleName: string | undefined, roles: Role[]): Role | undefined {
  return roles.find(role => role.name === roleName);
  }
