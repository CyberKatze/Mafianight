import { useState } from 'react';

interface Props {
  onStart: (
    numberOfPlayers: number,
    playerNames: string[],
    roles: { [key: string]: number }) => void;
}

const roles = ["Doctor", "Detective", "Mafia", "Citizen", "Sniper", "Godfather"];

const Gamesetup = ({ onStart }: Props) => {
  const [step, setStep] = useState(1);
  const [numberOfPlayers, setNumberOfPlayers] = useState(0);
  const [playerNames, setPlayerNames] = useState<string[]>([]);
  const [roleCounts, setRoleCounts] = useState<{ [key: string]: number }>({});
  const [error, setError] = useState<string | null>(null);

  const handleNext = () => {
    if (step === 1) {
      const totalRoles = Object.values(roleCounts).reduce((acc, count) => acc + count, 0);
      if (totalRoles > 0) {
        setNumberOfPlayers(totalRoles);
        const defaultNames = Array.from({ length: totalRoles }, (_, i) => `Player ${i + 1}`);
        setPlayerNames(defaultNames);
        setStep(2);
        setError(null);
      } else {
        setError('Please assign at least one role.');
      }
    } else if (step === 2 && playerNames.every(name => name.trim() !== '')) {
      onStart(numberOfPlayers, playerNames, roleCounts);
      setError(null);
    } else {
      setError('Please complete all fields.');
    }
  };

  const handlePrevious = () => {
    if (step > 1) {
      setStep(step - 1);
      setError(null);
    }
  };

  const handlePlayerNameChange = (index: number, name: string) => {
    const newPlayerNames = [...playerNames];
    newPlayerNames[index] = name;
    setPlayerNames(newPlayerNames);
  };

  const handleRoleCountChange = (role: string, increment: boolean) => {
    const currentCount = roleCounts[role] || 0;
    const newCount = increment ? currentCount + 1 : currentCount - 1;
    if (newCount >= 0) {
      setRoleCounts({
        ...roleCounts,
        [role]: newCount,
      });
    }
  };

  const totalRoles = Object.values(roleCounts).reduce((acc, count) => acc + count, 0);

  return (
    <div className="max-w-sm mx-auto bg-onyx border-onyx p-8 rounded-xl shadow-md">
      <div className="flex justify-between items-center mb-4">
        <h2 className="text-xl text-white font-semibold">Game Setup</h2>
        <div className="text-lavender ml-4 ">
          Players: {totalRoles}
        </div>
      </div>
      {error && <div className="text-red mb-4">{error}</div>}
      {step === 1 && (
        <>
          <h3 className="text-lg text-white font-semibold mb-2">Select Roles:</h3>
          {roles.map((role, index) => (
            <div key={index} className="mb-2 flex items-center">
              <label className="block text-sm font-medium text-white mb-1 w-1/3">
                {role}
              </label>
              <button
                onClick={() => handleRoleCountChange(role, false)}
                className="px-2 py-1 bg-slate text-white rounded-md mr-2"
              >
                -
              </button>
              <span className="text-white w-1/3 text-center">{roleCounts[role] || 0}</span>
              <button
                onClick={() => handleRoleCountChange(role, true)}
                className="px-2 py-1 bg-slate text-white rounded-md ml-2"
              >
                +
              </button>
            </div>
          ))}
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
                className="w-full px-3 py-2 border bg-slate border-slate text-gray-400 rounded-md"
              />
            </div>
          ))}
        </>
      )}
      <div className="mt-4 flex justify-between">
        {step > 1 && (
          <button
            onClick={handlePrevious}
            className="w-full bg-gray-500 hover:bg-gray-700 text-white py-2 rounded-md mr-2"
          >
            Previous
          </button>
        )}
        <button
          onClick={handleNext}
          className="w-full bg-violet hover:bg-lavender text-white py-2 rounded-md"
        >
          {step === 1 ? 'Next' : 'Start Game'}
        </button>
      </div>
    </div>
  );
};

export default Gamesetup;
