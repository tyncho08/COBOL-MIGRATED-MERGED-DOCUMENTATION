'use client'

import { InputHTMLAttributes, forwardRef, ReactNode } from 'react'

interface InputProps extends InputHTMLAttributes<HTMLInputElement> {
  label?: string
  error?: string
  helpText?: string
  leftIcon?: ReactNode
  rightIcon?: ReactNode
  variant?: 'default' | 'search'
}

const Input = forwardRef<HTMLInputElement, InputProps>(
  ({ className, type, label, error, helpText, leftIcon, rightIcon, variant = 'default', ...props }, ref) => {
    const baseInputClasses = 'block w-full rounded-md border-0 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6'
    const errorClasses = error ? 'ring-red-300 focus:ring-red-600' : ''
    const iconClasses = leftIcon || rightIcon ? 'pl-10' : 'pl-3'
    const rightIconClasses = rightIcon ? 'pr-10' : 'pr-3'

    return (
      <div className="w-full">
        {label && (
          <label className="block text-sm font-medium leading-6 text-gray-900 mb-2">
            {label}
          </label>
        )}
        <div className="relative">
          {leftIcon && (
            <div className="pointer-events-none absolute inset-y-0 left-0 flex items-center pl-3">
              <div className="h-5 w-5 text-gray-400">
                {leftIcon}
              </div>
            </div>
          )}
          <input
            type={type}
            className={`${baseInputClasses} ${errorClasses} ${iconClasses} ${rightIconClasses} ${className || ''}`}
            ref={ref}
            {...props}
          />
          {rightIcon && (
            <div className="pointer-events-none absolute inset-y-0 right-0 flex items-center pr-3">
              <div className="h-5 w-5 text-gray-400">
                {rightIcon}
              </div>
            </div>
          )}
        </div>
        {error && (
          <p className="mt-2 text-sm text-red-600">{error}</p>
        )}
        {helpText && !error && (
          <p className="mt-2 text-sm text-gray-500">{helpText}</p>
        )}
      </div>
    )
  }
)

Input.displayName = 'Input'

export default Input