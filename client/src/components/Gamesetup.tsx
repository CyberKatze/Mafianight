import React, { useState } from 'react'


interface Props {
  onStart: (
    numberOfPlayers: number, 
    playerNames: string[], 
    roles: { [key: string]: number }) => void;
}

const roles = ["Doctor", "Detective", "Mafia", "Citizen", "Sniper", "Godfather"];


const Gamesetup = ({onStart}:Props) => {
  const [step, setStep] = useState(1);
  const [numberOfPlayers, setNumberOfPlayers] = useState(0);
  const [playerNames, setPlayerNames] = useState<string[]>([]);
  const [roleCounts, setRoleCounts] = useState<{ [key: string]: number }>({});
  const [error, setError] = useState<string | null>(null); 

  const handleNext = () => {
    if (step === 1 && numberOfPlayers > 0) {
      const defaultNames = Array.from({ length: numberOfPlayers }, (_, i) => `Player ${i + 1}`);
      setPlayerNames(defaultNames);
      setStep(2);
      setError(null);
    } else if (step === 2 && playerNames.every(name => name.trim() !== '')) {
      setStep(3);
      setError(null);
    } else if (step === 3) {
      const totalRoles = Object.values(roleCounts).reduce((acc, count) => acc + count, 0);
      if (totalRoles === numberOfPlayers) {
        onStart(numberOfPlayers, playerNames, roleCounts);
        setError(null);
      } else {
        setError(`The total number of roles must be equal to the number of players (${numberOfPlayers}).`);
      }
    } else {
      setError('Please complete all fields.');
    }
  };

  const handlePlayerNameChange = (index: number, name: string) => {
    const newPlayerNames = [...playerNames];
    newPlayerNames[index] = name;
    setPlayerNames(newPlayerNames);
  };

  const handleRoleCountChange = (role: string, count: number) => {
    setRoleCounts({
      ...roleCounts,
      [role]: count,
    });
  };

  return (
    <div className="max-w-sm mx-auto bg-onyx border-onyx p-8 rounded-xl shadow-md">
      <h2 className="text-xl text-white font-semibold mb-4">Game Setup</h2>
      {error && <div className="text-red mb-4">{error}</div>}
      {step === 1 && (
        <>
          <label className="block text-sm font-medium text-white mb-2">
            Number of Players
          </label>
          <input
            type="number"
            value={numberOfPlayers}
            onChange={(e) => setNumberOfPlayers(Number(e.target.value))}
            className="w-full px-3 py-2  bg-slate border-slate placeholder-gray-400 text-gray-400  appearance-none focus:outline-none rounded-md"
            min="1"
          />
        </>
      )}
      {step === 2 && (
        <>
          {playerNames.map((name, index) => (
            <div key={index} className="mb-2">
              <label className="block text-sm font-medium text-white mb-1">
                Player {index + 1} Name
              </label>
              <input
                type="text"
                value={name}
                onChange={(e) => handlePlayerNameChange(index, e.target.value)}
                className="w-full px-3 py-2 border  bg-slate border-slate  text-gray-400   rounded-md"
              />
            </div>
          ))}
        </>
      )}
      {step === 3 && (
        <>
          <h3 className="text-lg text-white font-semibold mb-2">Select Roles:</h3>
          {roles.map((role, index) => (
            <div key={index} className="mb-2">
              <label className="block text-sm font-medium text-white mb-1">
                {role}
              </label>
              <input
                type="number"
                value={roleCounts[role] || 0}
                onChange={(e) => handleRoleCountChange(role, Number(e.target.value))}
                className="w-full px-3 py-2 bg-slate border-slate text-gray-400  rounded-md"
                min="0"
              />
            </div>
          ))}
        </>
      )}
      <button
        onClick={handleNext}
        className="mt-4 w-full bg-violet hover:bg-lavendor text-white py-2 rounded-md "
      >
        {step === 1 ? 'Next' : step === 2 ? 'Next' : 'Start Game'}
      </button>
    </div>
  );
};

export default Gamesetup