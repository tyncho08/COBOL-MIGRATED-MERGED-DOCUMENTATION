'use client'

import { useState, useEffect } from 'react'
import { 
  FolderIcon,
  DocumentTextIcon,
  CurrencyDollarIcon,
  UsersIcon,
  TruckIcon,
  CubeIcon,
  CalculatorIcon,
  PlusIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'

interface ReportCategory {
  id: string
  name: string
  description: string
  icon: string
  report_count: number
  last_updated: string
  color: string
}

export default function ReportCategoriesPage() {
  const [categories, setCategories] = useState<ReportCategory[]>([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    const fetchCategories = async () => {
      try {
        const response = await fetch('http://localhost:8000/api/v1/reports/categories')
        if (response.ok) {
          const data = await response.json()
          setCategories(data.categories || [])
        } else {
          // Fallback data
          setCategories([
            {
              id: 'financial',
              name: 'Financial Reports',
              description: 'P&L, Balance Sheet, Trial Balance, and financial statements',
              icon: 'CurrencyDollarIcon',
              report_count: 12,
              last_updated: '2025-01-15T09:30:00Z',
              color: 'bg-green-500'
            },
            {
              id: 'sales',
              name: 'Sales Reports',
              description: 'Customer analysis, aging reports, and sales performance',
              icon: 'UsersIcon',
              report_count: 8,
              last_updated: '2025-01-15T10:15:00Z',
              color: 'bg-blue-500'
            },
            {
              id: 'purchase',
              name: 'Purchase Reports',
              description: 'Supplier analysis, AP aging, and purchase performance',
              icon: 'TruckIcon',
              report_count: 6,
              last_updated: '2025-01-15T09:45:00Z',
              color: 'bg-purple-500'
            },
            {
              id: 'stock',
              name: 'Stock Reports',
              description: 'Inventory valuation, movement reports, and stock analysis',
              icon: 'CubeIcon',
              report_count: 9,
              last_updated: '2025-01-15T08:00:00Z',
              color: 'bg-orange-500'
            },
            {
              id: 'tax',
              name: 'Tax Reports',
              description: 'VAT returns, tax calculations, and compliance reports',
              icon: 'CalculatorIcon',
              report_count: 4,
              last_updated: '2025-01-12T16:30:00Z',
              color: 'bg-red-500'
            },
            {
              id: 'custom',
              name: 'Custom Reports',
              description: 'User-defined and customized reports',
              icon: 'DocumentTextIcon',
              report_count: 3,
              last_updated: '2025-01-10T14:20:00Z',
              color: 'bg-gray-500'
            }
          ])
        }
      } catch (error) {
        console.error('Failed to fetch report categories:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchCategories()
  }, [])

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        <FolderIcon className="h-4 w-4" />
        Manage Categories
      </Button>
      <Button size="sm">
        <PlusIcon className="h-4 w-4" />
        New Category
      </Button>
    </div>
  )

  const getIcon = (iconName: string) => {
    const iconMap: Record<string, any> = {
      'CurrencyDollarIcon': CurrencyDollarIcon,
      'UsersIcon': UsersIcon,
      'TruckIcon': TruckIcon,
      'CubeIcon': CubeIcon,
      'CalculatorIcon': CalculatorIcon,
      'DocumentTextIcon': DocumentTextIcon
    }
    const IconComponent = iconMap[iconName] || DocumentTextIcon
    return <IconComponent className="h-8 w-8 text-white" />
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Report Categories"
        description="Organize and browse reports by category"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Reports', href: '/reports' },
          { label: 'Categories' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {loading ? (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {Array.from({ length: 6 }).map((_, index) => (
              <div key={index} className="animate-pulse">
                <div className="bg-white rounded-lg shadow p-6">
                  <div className="flex items-center">
                    <div className="h-12 w-12 bg-gray-300 rounded-lg"></div>
                    <div className="ml-4 flex-1">
                      <div className="h-4 bg-gray-300 rounded w-3/4 mb-2"></div>
                      <div className="h-3 bg-gray-300 rounded w-1/2"></div>
                    </div>
                  </div>
                  <div className="mt-4">
                    <div className="h-3 bg-gray-300 rounded mb-2"></div>
                    <div className="h-3 bg-gray-300 rounded w-4/5"></div>
                  </div>
                </div>
              </div>
            ))}
          </div>
        ) : (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {categories.map((category) => (
              <Card key={category.id} className="hover:shadow-lg transition-shadow cursor-pointer">
                <div className="p-6">
                  <div className="flex items-center">
                    <div className={`p-3 rounded-lg ${category.color}`}>
                      {getIcon(category.icon)}
                    </div>
                    <div className="ml-4 flex-1">
                      <h3 className="text-lg font-medium text-gray-900">
                        {category.name}
                      </h3>
                      <p className="text-sm text-gray-500">
                        {category.report_count} report{category.report_count !== 1 ? 's' : ''}
                      </p>
                    </div>
                  </div>
                  
                  <p className="mt-4 text-sm text-gray-600">
                    {category.description}
                  </p>
                  
                  <div className="mt-4 flex items-center justify-between">
                    <span className="text-xs text-gray-500">
                      Updated {new Date(category.last_updated).toLocaleDateString()}
                    </span>
                    <Button 
                      variant="outline" 
                      size="sm"
                      onClick={() => window.location.href = `/reports/all?category=${category.id}`}
                    >
                      View Reports
                    </Button>
                  </div>
                </div>
              </Card>
            ))}
          </div>
        )}

        {/* Category Statistics */}
        <div className="mt-12">
          <Card>
            <div className="px-6 py-4 border-b border-gray-200">
              <h3 className="text-lg font-medium text-gray-900">Category Statistics</h3>
            </div>
            <div className="p-6">
              <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                <div className="text-center">
                  <div className="text-2xl font-bold text-gray-900">
                    {categories.length}
                  </div>
                  <div className="text-sm text-gray-500">Total Categories</div>
                </div>
                <div className="text-center">
                  <div className="text-2xl font-bold text-gray-900">
                    {categories.reduce((sum, cat) => sum + cat.report_count, 0)}
                  </div>
                  <div className="text-sm text-gray-500">Total Reports</div>
                </div>
                <div className="text-center">
                  <div className="text-2xl font-bold text-gray-900">
                    {Math.round(categories.reduce((sum, cat) => sum + cat.report_count, 0) / categories.length) || 0}
                  </div>
                  <div className="text-sm text-gray-500">Avg per Category</div>
                </div>
                <div className="text-center">
                  <div className="text-2xl font-bold text-gray-900">
                    {categories.find(cat => cat.report_count === Math.max(...categories.map(c => c.report_count)))?.name.split(' ')[0] || 'N/A'}
                  </div>
                  <div className="text-sm text-gray-500">Largest Category</div>
                </div>
              </div>
            </div>
          </Card>
        </div>
      </main>
    </div>
  )
}