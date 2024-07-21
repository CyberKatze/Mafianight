import { test, expect } from '@playwright/test';
import { shuffle, distribute_roles } from '../../src/lib/utils';

test('shuffle function', () => {
  const array = ['1', '2', '3', '4', '5'];
  const shuffled = shuffle(array);
  expect(shuffled).not.toEqual(array);
  expect(shuffled).toHaveLength(array.length);
  expect(shuffled).toEqual(expect.arrayContaining(array));
});

test('distribution of roles', () => {
  const players = [{ name: 'Alice' }, { name: 'Bob' }, { name: 'Charlie' }];
  const roles = { 'villager': 2, 'werewolf': 1 };
  const playersWithRoles = distribute_roles(players, roles);
  expect(playersWithRoles).toHaveLength(players.length);

  playersWithRoles.forEach(player => {
    roles[player.role as string]--;
    expect(player).toHaveProperty('role')
  })
  expect(Object.values(roles)).toEqual([0, 0]);
});
