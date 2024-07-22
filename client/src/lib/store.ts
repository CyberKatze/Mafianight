import { atomWithImmer } from 'jotai-immer';
import { atom } from 'jotai';
import { createStore } from 'jotai';
import type { Game, Role } from './types';

const gameAtom = atom<Game>({ id: undefined, name: '', players: [] });
const gameStore = createStore();

gameStore.set(gameAtom, x => x)

const rolesAtom = atomWithImmer<Role[]>([]);

export { gameAtom, rolesAtom, gameStore }



