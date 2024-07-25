import { zodResolver } from '@hookform/resolvers/zod';
import axios from 'axios';
import { useCallback, useState } from 'react';
import { useForm } from 'react-hook-form';
import { useNavigate } from 'react-router-dom';
import { z } from 'zod';
import hat from "../assets/images/hat.png";

// Define the schema for login
const schema = z.object({
  email: z.string().email({ message: 'Invalid email address' }),
  password: z.string().min(6, { message: 'Password must be at least 6 characters long' }),
});

// Infer the type using Zod
type FormData = z.infer<typeof schema>;

const LoginForm = () => {
  const navigate = useNavigate();
  const { register, handleSubmit, formState: { errors } } = useForm<FormData>({
    resolver: zodResolver(schema),
  });
  const apiUrl = import.meta.env.VITE_API_URL;
  const[error, setError] = useState<string | null>(null);

  const onSubmit = useCallback(async (data: FormData) => {
    try {
      const response = await axios.post(apiUrl + '/login', {
        email: data.email,
        password: data.password,
      });
      console.log('Login successful:', response.data);
      localStorage.setItem('token', response.data.bearerToken);
      navigate('/');
    } catch (error) {
      console.error('Error:', error);
      if (axios.isAxiosError(error) && error.response) {
        setError(error.response.data.message || 'An error occurred during log-in');
      } else {
        setError('An unknown error occurred');
      }
    }
  }, [navigate, apiUrl]);

  return (
    <section>
      <div className="flex flex-col items-center justify-center px-6 py-8 mx-auto md:h-screen lg:py-0">
        <a href="#" className="flex items-center mb-6 text-2xl font-semibold text-text-default">
          <img className="w-8 h-8 mr-2 bg-transparent rounded-full" src={hat} alt="logo" />
          Mafia
        </a>
        <div className="w-full rounded-lg shadow border md:mt-0 sm:max-w-md xl:p-0 bg-gradient-to-t from-mantle to-crust border-onyx">
          <div className="p-6 space-y-4 md:space-y-6 sm:p-8">
            <h1 className="text-xl font-bold leading-tight tracking-tight text-text-default md:text-2xl">
              Sign in to your account
            </h1>
            <form className="space-y-4 md:space-y-6" onSubmit={handleSubmit(onSubmit)}>
              <div>
                <label htmlFor="email" className="block mb-2 text-sm font-medium text-text-default">Your email</label>
                <input
                  type="email"
                  id="email"
                  {...register('email')}
                  className={`border sm:text-sm rounded-lg block w-full p-2.5 bg-surface-dark border-slate placeholder-gray-400 text-text-default focus:ring-blue-500 focus:border-blue-500 ${errors.email ? 'border-red-500' : ''}`}
                  placeholder="name@.com"
                  required
                />
                {errors.email && <p className="text-red text-sm mt-1">{errors.email.message}</p>}
              </div>
              <div>
                <label htmlFor="password" className="block mb-2 text-sm font-medium text-text-default">Password</label>
                <input
                  type="password"
                  id="password"
                  {...register('password')}
                  className={`border sm:text-sm rounded-lg block w-full p-2.5 bg-surface-dark border-slate placeholder-gray-400 text-text-default focus:ring-blue-500 focus:border-blue-500 ${errors.password ? 'border-red-500' : ''}`}
                  placeholder="••••••••"
                  required
                />
                {errors.password && <p className="text-red text-sm mt-1">{errors.password.message}</p>}
              </div>
              <button type="submit" className="w-full text-text-inverted bg-gradient-to-r from-mauve to-lavender font-semibold hover:bg-lavendor focus:ring-4 focus:outline-none focus:ring-blue-300  rounded-lg text-sm px-5 py-2.5 text-center dark:bg-primary-600 dark:focus:ring-primary-800">
                Sign in
              </button>
              {error && <p className="text-red text-sm mt-1">{error}</p>}
              <p className="text-sm font-light text-gray-400">
                Don’t have an account yet? <a href="/signup" className="font-medium hover:underline text-primary-500">Sign up</a>
              </p>
            </form>
          </div>
        </div>
      </div>
    </section>
  );
};

export default LoginForm;
