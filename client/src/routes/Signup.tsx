import { zodResolver } from '@hookform/resolvers/zod';
import axios from 'axios';
import { useCallback } from 'react';
import { useForm } from 'react-hook-form';
import { useNavigate } from 'react-router-dom';
import { z } from 'zod';
import hat from "../assets/images/hat.png";

// Define the schema for signup
const schema = z.object({
  email: z.string().email({ message: 'Invalid email address' }),
  password: z.string().min(6, { message: 'Password must be at least 6 characters long' }),
  username: z.string().min(3, { message: 'Username must be at least 3 characters long' }),
  confirmPassword: z.string().min(6, { message: 'Password must be at least 6 characters long' })
}).refine((data) => data.password === data.confirmPassword, {
  message: "Passwords don't match",
  path: ["confirmPassword"],
});

// Infer the type using Zod
type FormData = z.infer<typeof schema>;

const Signup = () => {
  const navigate = useNavigate();
  const { register, handleSubmit, formState: { errors } } = useForm<FormData>({
    resolver: zodResolver(schema),
  });
  
  const apiUrl = import.meta.env.VITE_API_URL;

  const onSubmit = useCallback(async (data: FormData) => {
    try {
      const response = await axios.post(apiUrl + '/register', {
        email: data.email,
        userName: data.username,
        password: data.password,
      });
      console.log('Sign-up successful:', response.data);
      navigate('/login');
    } catch (error) {
      console.error('Error:', error);
    }
  }, [navigate, apiUrl]);

  return (
    <section>
      <div className="flex flex-col items-center justify-center px-6 py-8 mx-auto md:h-screen lg:py-0">
        <a href="#" className="flex items-center mb-6 text-2xl font-semibold text-white">
          <img className="w-8 h-8 mr-2 bg-black rounded-full" src={hat} alt="logo" />
          Mafia
        </a>
        <div className="w-full rounded-lg shadow border md:mt-0 sm:max-w-md xl:p-0 bg-onyx border-onyx">
          <div className="p-6 space-y-4 md:space-y-6 sm:p-8">
            <h1 className="text-xl font-bold leading-tight tracking-tight text-white md:text-2xl">
              Create an account
            </h1>
            <form className="space-y-4 md:space-y-6" onSubmit={handleSubmit(onSubmit)}>
              <div>
                <label htmlFor="email" className="block mb-2 text-sm font-medium text-white">Your email</label>
                <input
                  type="email"
                  id="email"
                  {...register('email')}
                  className={`border sm:text-sm rounded-lg block w-full p-2.5 bg-slate border-slate placeholder-gray-400 text-white focus:ring-blue-500 focus:border-blue-500 ${errors.email ? 'border-red-500' : ''}`}
                  placeholder="name@.com"
                  required
                />
                {errors.email && <p className="text-red text-sm mt-1">{errors.email.message}</p>}
              </div>
              <div>
                <label htmlFor="username" className="block mb-2 text-sm font-medium text-white">Your username</label>
                <input
                  type="text"
                  id="username"
                  {...register('username')}
                  className={`border sm:text-sm rounded-lg block w-full p-2.5 bg-slate border-slate placeholder-gray-400 text-white focus:ring-blue-500 focus:border-blue-500 ${errors.username ? 'border-red-500' : ''}`}
                  placeholder="Your username"
                  required
                />
                {errors.username && <p className="text-red text-sm mt-1">{errors.username.message}</p>}
              </div>
              <div>
                <label htmlFor="password" className="block mb-2 text-sm font-medium text-white">Password</label>
                <input
                  type="password"
                  id="password"
                  {...register('password')}
                  className={`border sm:text-sm rounded-lg block w-full p-2.5 bg-slate border-slate placeholder-gray-400 text-white focus:ring-blue-500 focus:border-blue-500 ${errors.password ? 'border-red-500' : ''}`}
                  placeholder="••••••••"
                  required
                />
                {errors.password && <p className="text-red text-sm mt-1">{errors.password.message}</p>}
              </div>
              <div>
                <label htmlFor="confirmPassword" className="block mb-2 text-sm font-medium text-white">Confirm Password</label>
                <input
                  type="password"
                  id="confirmPassword"
                  {...register('confirmPassword')}
                  className={`border sm:text-sm rounded-lg block w-full p-2.5 bg-slate border-slate placeholder-gray-400 text-white focus:ring-blue-500 focus:border-blue-500 ${errors.confirmPassword ? 'border-red-500' : ''}`}
                  placeholder="••••••••"
                  required
                />
                {errors.confirmPassword && <p className="text-red text-sm mt-1">{errors.confirmPassword.message}</p>}
              </div>
              <button type="submit" className="w-full text-white bg-violet hover:bg-lavendor focus:ring-4 focus:outline-none focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center dark:bg-primary-600 dark:focus:ring-primary-800">
                Sign up
              </button>
              <p className="text-sm font-light text-gray-400">
                Already have an account? <a href="/login" className="font-medium hover:underline text-primary-500">Sign in</a>
              </p>
            </form>
          </div>
        </div>
      </div>
    </section>
  );
};

export default Signup;
