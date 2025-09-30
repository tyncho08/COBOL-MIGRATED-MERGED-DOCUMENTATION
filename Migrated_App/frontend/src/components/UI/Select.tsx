'use client'

import { forwardRef, SelectHTMLAttributes } from 'react'
import { clsx } from 'clsx'

interface Option {
  value: string
  label: string
}

interface SelectProps extends SelectHTMLAttributes<HTMLSelectElement> {
  label?: string
  error?: string
  helperText?: string
  options: Option[] | { value: string; label: string }[]
  size?: 'xs' | 'sm' | 'md' | 'lg'
}

const Select = forwardRef<HTMLSelectElement, SelectProps>(
  ({ label, error, helperText, options, size = 'md', className, ...props }, ref) => {
    const sizeClasses = {
      xs: 'py-1 px-2 text-xs',
      sm: 'py-1.5 px-2.5 text-sm',
      md: 'py-2 px-3 text-sm',
      lg: 'py-2.5 px-3.5 text-base',
    }

    return (
      <div>
        {label && (
          <label className="block text-sm font-medium text-gray-700 mb-1">
            {label}
          </label>
        )}
        <select
          ref={ref}
          className={clsx(
            'block w-full rounded-md border-gray-300 shadow-sm',
            'focus:border-indigo-500 focus:ring-indigo-500',
            'disabled:bg-gray-100 disabled:cursor-not-allowed',
            sizeClasses[size],
            {
              'border-red-300 focus:border-red-500 focus:ring-red-500': error,
            },
            className
          )}
          {...props}
        >
          {options.map((option) => (
            <option key={option.value} value={option.value}>
              {option.label}
            </option>
          ))}
        </select>
        {error && (
          <p className="mt-1 text-sm text-red-600">{error}</p>
        )}
        {helperText && !error && (
          <p className="mt-1 text-sm text-gray-500">{helperText}</p>
        )}
      </div>
    )
  }
)

Select.displayName = 'Select'

export default Select