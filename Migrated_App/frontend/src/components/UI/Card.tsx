'use client'

import { ReactNode } from 'react'

interface CardProps {
  children: ReactNode
  className?: string
  padding?: 'none' | 'sm' | 'md' | 'lg'
}

interface CardHeaderProps {
  children: ReactNode
  className?: string
}

interface CardBodyProps {
  children: ReactNode
  className?: string
}

interface CardFooterProps {
  children: ReactNode
  className?: string
}

export function Card({ children, className = '', padding = 'md' }: CardProps) {
  const paddingClasses = {
    none: '',
    sm: 'p-3',
    md: 'p-6',
    lg: 'p-8'
  }

  return (
    <div className={`bg-white overflow-hidden shadow rounded-lg border border-gray-200 ${paddingClasses[padding]} ${className}`}>
      {children}
    </div>
  )
}

export function CardHeader({ children, className = '' }: CardHeaderProps) {
  return (
    <div className={`px-6 py-4 border-b border-gray-200 bg-gray-50 ${className}`}>
      {children}
    </div>
  )
}

export function CardBody({ children, className = '' }: CardBodyProps) {
  return (
    <div className={`px-6 py-4 ${className}`}>
      {children}
    </div>
  )
}

export function CardFooter({ children, className = '' }: CardFooterProps) {
  return (
    <div className={`px-6 py-4 border-t border-gray-200 bg-gray-50 ${className}`}>
      {children}
    </div>
  )
}

// Stats Card Component
interface StatsCardProps {
  title: string
  value: string | number
  change?: {
    value: string
    type: 'increase' | 'decrease' | 'neutral'
  }
  icon?: ReactNode
  href?: string
}

export function StatsCard({ title, value, change, icon, href }: StatsCardProps) {
  const changeColorClasses = {
    increase: 'text-green-600',
    decrease: 'text-red-600',
    neutral: 'text-gray-600'
  }

  const CardContent = (
    <Card className="hover:shadow-md transition-shadow">
      <div className="flex items-center">
        {icon && (
          <div className="flex-shrink-0">
            <div className="flex items-center justify-center h-12 w-12 rounded-md bg-indigo-500 text-white">
              {icon}
            </div>
          </div>
        )}
        <div className={`${icon ? 'ml-4' : ''} flex-1`}>
          <dt className="text-sm font-medium text-gray-500 truncate">{title}</dt>
          <dd className="text-2xl font-bold text-gray-900">{value}</dd>
          {change && (
            <dd className={`text-sm ${changeColorClasses[change.type]}`}>
              {change.value}
            </dd>
          )}
        </div>
      </div>
    </Card>
  )

  if (href) {
    return (
      <a href={href} className="block">
        {CardContent}
      </a>
    )
  }

  return CardContent
}

// Table Card Component
interface TableCardProps {
  title: string
  description?: string
  children: ReactNode
  actions?: ReactNode
}

export function TableCard({ title, description, children, actions }: TableCardProps) {
  return (
    <Card padding="none">
      <CardHeader>
        <div className="flex items-center justify-between">
          <div>
            <h3 className="text-lg font-medium text-gray-900">{title}</h3>
            {description && (
              <p className="mt-1 text-sm text-gray-500">{description}</p>
            )}
          </div>
          {actions && <div>{actions}</div>}
        </div>
      </CardHeader>
      <CardBody className="p-0">
        {children}
      </CardBody>
    </Card>
  )
}