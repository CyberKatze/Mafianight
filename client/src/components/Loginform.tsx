import { useForm } from 'react-hook-form';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import hat from "../assets/images/hat.png"
import { useState } from 'react';


const loginSchema = z.object({
  email: z.string().email({ message: 'Invalid email address' }),
  password: z.string().min(6, { message: 'Password must be at least 6 characters long' }),
  confirmPassword: z.string().min(6, { message: 'Password must be at least 6 characters long' })
});
const signupSchema = z.object({
  email: z.string().email({ message: 'Invalid email address' }),
  password: z.string().min(6, { message: 'Password must be at least 6 characters long' }),
  confirmPassword: z.string().min(6, { message: 'Password must be at least 6 characters long' }),
}).refine((data) => data.password === data.confirmPassword, {
  message: "Passwords don't match",
  path: ["confirmPassword"],
});


type LoginFormData = z.infer<typeof loginSchema>;
type SignupFormData = z.infer<typeof signupSchema>;

type FormData = LoginFormData | SignupFormData;


const Loginform = () => {
  const [isSignUp, setIsSignUp] = useState(false);
  const formOptions = isSignUp ? { resolver: zodResolver(loginSchema) } : { resolver: zodResolver(signupSchema) };

  const { register, handleSubmit, formState: { errors } } = useForm<FormData>(formOptions);


  const onSubmit = (data: FormData) => {
    console.log(data);
  };

  return (
    <section >
      <div className="flex flex-col items-center justify-center px-6 py-8 mx-auto md:h-screen lg:py-0">
        <a href="#" className="flex items-center mb-6 text-2xl font-semibold  text-white">
          <img className="w-8 h-8 mr-2 bg-black rounded-full" src={hat} alt="logo" />
          Mafia
        </a>
        <div className="w-full  rounded-lg shadow border md:mt-0 sm:max-w-md xl:p-0 bg-onyx border-onyx">
          <div className="p-6 space-y-4 md:space-y-6 sm:p-8">
            <h1 className="text-xl font-bold leading-tight tracking-tight  md:text-2xl text-white">
              {isSignUp ? 'Create an account' : 'Sign in to your account'}
            </h1>
            <form className="space-y-4 md:space-y-6" onSubmit={handleSubmit(onSubmit)}>
              <div>
                <label htmlFor="email" className="block mb-2 text-sm font-medium  text-white">Your email</label>
                <input
                  type="email"
                  id="email"
                  {...register('email')}
                  className={` border sm:text-sm rounded-lg  block w-full p-2.5 bg-slate border-slate placeholder-gray-400 text-white focus:ring-blue-500 focus:border-blue-500 ${errors.email ? 'border-red-500' : ''}`}
                  placeholder="name@.com"
                  required
                />
                {errors.email && <p className="text-red text-sm mt-1">{errors.email.message}</p>}
              </div>
              <div>
                <label htmlFor="password" className="block mb-2 text-sm font-medium  text-white">Password</label>
                <input
                  type="password"
                  id="password"
                  {...register('password')}
                  className={` border  sm:text-sm rounded-lg  block w-full p-2.5 bg-slate border-slate placeholder-gray-400 text-white focus:ring-blue-500 focus:border-blue-500 ${errors.password ? 'border-red-500' : ''}`}
                  placeholder="••••••••"
                  required
                />
                {errors.password && <p className="text-red text-sm mt-1">{errors.password.message}</p>}
              </div>
              {isSignUp && ( 
                <div>
                  <label htmlFor="confirmPassword" className="block mb-2 text-sm font-medium text-white">Confirm Password</label>
                  <input
                    type="password"
                    id="confirmPassword"
                    {...register('confirmPassword')}
                    className={` border  sm:text-sm rounded-lg focus:ring-primary-600 focus:border-primary-600 block w-full p-2.5 bg-slate border-slate placeholder-gray-400 text-white focus:ring-blue-500 focus:border-blue-500 ${errors.confirmPassword ? 'border-red-500' : ''}`}
                    placeholder="••••••••"
                    required
                  />
                  {errors.confirmPassword && <p className="text-red text-sm mt-1">{errors.confirmPassword.message}</p>}
                </div>
              )}
              <button  type="submit" className="w-full text-white bg-violet hover:bg-lavendor focus:ring-4 focus:outline-none focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center dark:bg-primary-600 dark:focus:ring-primary-800">{isSignUp ?  'Sign up' : 'Sign in'}</button>
              <p className="text-sm font-light text-gray-400">
              {isSignUp ? 'Already have an account?' : "Don’t have an account yet?"} <a onClick={() => setIsSignUp(!isSignUp)} href="#" className="font-medium  hover:underline text-primary-500">{isSignUp ? 'Sign in' : 'Sign up'}</a>
              </p>
            </form>
          </div>
        </div>
      </div>
    </section>
  );
} 
export default Loginform
