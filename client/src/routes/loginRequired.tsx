import React from 'react';
import { useNavigate } from 'react-router-dom';

const LoginRequired: React.FC = () => {
  const navigate = useNavigate();

  const handleLoginRedirect = () => {
    navigate('/login');
  };

  return (
    <div className="flex flex-col items-center justify-center min-h-screen text-center bg-dark">
      <div className="bg-darkgray p-8 rounded-lg shadow-lg border-2 border-violet max-w-lg mx-auto">
        <h1 className="text-2xl font-bold mb-4 text-white">Access Denied</h1>
        <p className="mb-4 text-white">You need to be logged in to access this page.</p>
        <button
          onClick={handleLoginRedirect}
          className="bg-violet hover:bg-lavender text-white font-bold py-2 px-4 rounded shadow-md hover:shadow-lg transition duration-300"
        >
          Go to Login
        </button>
      </div>
    </div>
  );
};

export default LoginRequired;
