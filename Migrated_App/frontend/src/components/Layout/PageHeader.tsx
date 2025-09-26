'use client'

import { ReactNode } from 'react'

interface PageHeaderProps {
  title: string
  description?: string
  actions?: ReactNode
  breadcrumbs?: Array<{
    label: string
    href?: string
  }>
}

export default function PageHeader({ title, description, actions, breadcrumbs }: PageHeaderProps) {
  return (
    <div className="border-b border-gray-200 bg-white px-4 py-4 sm:px-6 lg:px-8">
      <div className="flex items-center justify-between">
        <div className="min-w-0 flex-1">
          {/* Breadcrumbs */}
          {breadcrumbs && breadcrumbs.length > 0 && (
            <nav className="flex mb-2" aria-label="Breadcrumb">
              <ol className="flex items-center space-x-2">
                {breadcrumbs.map((crumb, index) => (
                  <li key={index} className="flex items-center">
                    {index > 0 && (
                      <svg
                        className="h-4 w-4 text-gray-400 mx-2"
                        fill="currentColor"
                        viewBox="0 0 20 20"
                      >
                        <path
                          fillRule="evenodd"
                          d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                          clipRule="evenodd"
                        />
                      </svg>
                    )}
                    {crumb.href ? (
                      <a
                        href={crumb.href}
                        className="text-sm font-medium text-gray-500 hover:text-gray-700"
                      >
                        {crumb.label}
                      </a>
                    ) : (
                      <span className="text-sm font-medium text-gray-900">{crumb.label}</span>
                    )}
                  </li>
                ))}
              </ol>
            </nav>
          )}

          {/* Title and Description */}
          <h1 className="text-2xl font-bold leading-7 text-gray-900 sm:truncate sm:text-3xl sm:tracking-tight">
            {title}
          </h1>
          {description && (
            <p className="mt-1 text-sm text-gray-500">{description}</p>
          )}
        </div>

        {/* Actions */}
        {actions && (
          <div className="ml-4 mt-4 flex-shrink-0 sm:mt-0">
            {actions}
          </div>
        )}
      </div>
    </div>
  )
}