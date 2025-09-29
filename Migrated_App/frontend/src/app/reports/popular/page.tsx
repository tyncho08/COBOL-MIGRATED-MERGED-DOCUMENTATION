'use client'

import { useState, useEffect } from 'react'
import { 
  ChartBarIcon,
  ArrowUpIcon,
  EyeIcon,
  ArrowDownTrayIcon,
  CalendarIcon,
  UsersIcon,
  DocumentTextIcon,
  ClockIcon
} from '@heroicons/react/24/outline'
import { StarIcon } from '@heroicons/react/24/solid'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import Table from '@/components/UI/Table'

interface PopularReport {
  id: string
  name: string
  description: string
  category: string
  view_count: number
  download_count: number
  unique_users: number
  last_accessed: string
  trend: 'up' | 'down' | 'stable'
  trend_percentage: number
  avg_generation_time: string
  rating: number
  last_generated: string
}

export default function PopularReportsPage() {
  const [popularReports, setPopularReports] = useState<PopularReport[]>([])
  const [loading, setLoading] = useState(true)
  const [filterCategory, setFilterCategory] = useState('all')
  const [sortBy, setSortBy] = useState('view_count')

  useEffect(() => {
    const fetchPopularReports = async () => {
      try {
        const response = await fetch('http://localhost:8000/api/v1/reports/popular')
        if (response.ok) {
          const data = await response.json()
          setPopularReports(data.reports || [])
        } else {
          // Fallback data
          setPopularReports([
            {
              id: 'pop-001',
              name: 'Trial Balance',
              description: 'Complete trial balance with all GL accounts and balances',
              category: 'Financial',
              view_count: 284,
              download_count: 156,
              unique_users: 23,
              last_accessed: '2025-01-15T14:30:00Z',
              trend: 'up',
              trend_percentage: 12.5,
              avg_generation_time: '2.3 seconds',
              rating: 4.8,
              last_generated: '2025-01-15T09:30:00Z'
            },
            {
              id: 'pop-002',
              name: 'Customer Aging Report',
              description: 'Outstanding receivables by aging buckets',
              category: 'Sales',
              view_count: 198,
              download_count: 134,
              unique_users: 18,
              last_accessed: '2025-01-15T13:45:00Z',
              trend: 'up',
              trend_percentage: 8.7,
              avg_generation_time: '1.8 seconds',
              rating: 4.6,
              last_generated: '2025-01-15T10:15:00Z'
            },
            {
              id: 'pop-003',
              name: 'Stock Valuation Report',
              description: 'Inventory valuation by location and category',
              category: 'Stock',
              view_count: 167,
              download_count: 89,
              unique_users: 15,
              last_accessed: '2025-01-15T12:20:00Z',
              trend: 'stable',
              trend_percentage: 0.2,
              avg_generation_time: '4.1 seconds',
              rating: 4.4,
              last_generated: '2025-01-15T08:00:00Z'
            },
            {
              id: 'pop-004',
              name: 'P&L Statement',
              description: 'Income statement showing revenue and expenses',
              category: 'Financial',
              view_count: 145,
              download_count: 98,
              unique_users: 12,
              last_accessed: '2025-01-15T11:30:00Z',
              trend: 'down',
              trend_percentage: -3.2,
              avg_generation_time: '3.2 seconds',
              rating: 4.7,
              last_generated: '2025-01-15T08:45:00Z'
            },
            {
              id: 'pop-005',
              name: 'Sales Analysis Report',
              description: 'Sales performance by customer, product, and territory',
              category: 'Sales',
              view_count: 132,
              download_count: 76,
              unique_users: 14,
              last_accessed: '2025-01-15T10:45:00Z',
              trend: 'up',
              trend_percentage: 15.3,
              avg_generation_time: '5.7 seconds',
              rating: 4.3,
              last_generated: '2025-01-15T07:30:00Z'
            },
            {
              id: 'pop-006',
              name: 'Supplier Aging Report',
              description: 'Outstanding payables by aging buckets',
              category: 'Purchase',
              view_count: 98,
              download_count: 67,
              unique_users: 9,
              last_accessed: '2025-01-15T09:15:00Z',
              trend: 'stable',
              trend_percentage: 1.1,
              avg_generation_time: '2.1 seconds',
              rating: 4.2,
              last_generated: '2025-01-15T09:45:00Z'
            },
            {
              id: 'pop-007',
              name: 'VAT Return',
              description: 'VAT calculations and returns for compliance',
              category: 'Tax',
              view_count: 76,
              download_count: 45,
              unique_users: 7,
              last_accessed: '2025-01-14T16:00:00Z',
              trend: 'down',
              trend_percentage: -8.9,
              avg_generation_time: '1.5 seconds',
              rating: 4.5,
              last_generated: '2025-01-10T14:30:00Z'
            }
          ])
        }
      } catch (error) {
        console.error('Failed to fetch popular reports:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchPopularReports()
  }, [])

  const filteredReports = popularReports
    .filter(report => filterCategory === 'all' || report.category.toLowerCase() === filterCategory.toLowerCase())
    .sort((a, b) => {
      switch (sortBy) {
        case 'view_count':
          return b.view_count - a.view_count
        case 'download_count':
          return b.download_count - a.download_count
        case 'unique_users':
          return b.unique_users - a.unique_users
        case 'rating':
          return b.rating - a.rating
        default:
          return b.view_count - a.view_count
      }
    })

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        <ChartBarIcon className="h-4 w-4" />
        Analytics Dashboard
      </Button>
      <Button size="sm">
        <DocumentTextIcon className="h-4 w-4" />
        Generate Report
      </Button>
    </div>
  )

  const getTrendIcon = (trend: string) => {
    switch (trend) {
      case 'up':
        return <ArrowUpIcon className="h-4 w-4 text-green-500" />
      case 'down':
        return <ArrowUpIcon className="h-4 w-4 text-red-500 transform rotate-180" />
      default:
        return <div className="h-4 w-4 bg-gray-400 rounded-full"></div>
    }
  }

  const getTrendColor = (trend: string) => {
    switch (trend) {
      case 'up':
        return 'text-green-600'
      case 'down':
        return 'text-red-600'
      default:
        return 'text-gray-600'
    }
  }

  const renderStars = (rating: number) => {
    return (
      <div className="flex items-center">
        {Array.from({ length: 5 }).map((_, index) => (
          <StarIcon
            key={index}
            className={`h-4 w-4 ${
              index < Math.floor(rating)
                ? 'text-yellow-400 fill-current'
                : 'text-gray-300'
            }`}
          />
        ))}
        <span className="ml-1 text-sm text-gray-600">{rating}</span>
      </div>
    )
  }

  const columns = [
    {
      key: 'rank',
      header: '#',
      className: 'w-12',
      render: (value: any, row: PopularReport, index: number) => (
        <div className="text-center">
          <span className={`inline-flex items-center justify-center h-6 w-6 rounded-full text-xs font-medium ${
            index === 0 ? 'bg-yellow-100 text-yellow-800' :
            index === 1 ? 'bg-gray-100 text-gray-800' :
            index === 2 ? 'bg-orange-100 text-orange-800' :
            'bg-blue-100 text-blue-800'
          }`}>
            {index + 1}
          </span>
        </div>
      )
    },
    {
      key: 'name',
      header: 'Report Name',
      className: 'min-w-[200px]',
      render: (value: any, row: PopularReport) => (
        <div>
          <div className="font-medium text-gray-900">{value}</div>
          <div className="text-sm text-gray-500">{row.description}</div>
        </div>
      )
    },
    {
      key: 'category',
      header: 'Category',
      className: 'w-24',
      render: (value: any) => (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-indigo-100 text-indigo-800">
          {value}
        </span>
      )
    },
    {
      key: 'view_count',
      header: 'Views',
      className: 'w-20 text-center',
      render: (value: any, row: PopularReport) => (
        <div className="text-center">
          <div className="text-sm font-medium text-gray-900">{value}</div>
          <div className="flex items-center justify-center">
            {getTrendIcon(row.trend)}
            <span className={`text-xs ml-1 ${getTrendColor(row.trend)}`}>
              {row.trend_percentage > 0 ? '+' : ''}{row.trend_percentage}%
            </span>
          </div>
        </div>
      )
    },
    {
      key: 'download_count',
      header: 'Downloads',
      className: 'w-24 text-center',
      render: (value: any) => (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
          {value}
        </span>
      )
    },
    {
      key: 'unique_users',
      header: 'Users',
      className: 'w-20 text-center',
      render: (value: any) => (
        <div className="flex items-center justify-center">
          <UsersIcon className="h-4 w-4 text-gray-400 mr-1" />
          <span className="text-sm text-gray-900">{value}</span>
        </div>
      )
    },
    {
      key: 'rating',
      header: 'Rating',
      className: 'w-32',
      render: (value: any) => renderStars(value)
    },
    {
      key: 'avg_generation_time',
      header: 'Avg Time',
      className: 'w-24',
      render: (value: any) => (
        <div className="flex items-center">
          <ClockIcon className="h-4 w-4 text-gray-400 mr-1" />
          <span className="text-sm text-gray-600">{value}</span>
        </div>
      )
    },
    {
      key: 'actions',
      header: 'Actions',
      className: 'w-24',
      render: (value: any, row: PopularReport) => (
        <div className="flex space-x-1">
          <Button variant="outline" size="xs">
            <EyeIcon className="h-3 w-3" />
          </Button>
          <Button variant="outline" size="xs">
            <ArrowDownTrayIcon className="h-3 w-3" />
          </Button>
        </div>
      )
    }
  ]

  const totalViews = popularReports.reduce((sum, r) => sum + r.view_count, 0)
  const totalDownloads = popularReports.reduce((sum, r) => sum + r.download_count, 0)
  const avgRating = popularReports.reduce((sum, r) => sum + r.rating, 0) / popularReports.length || 0
  const totalUsers = popularReports.reduce((sum, r) => sum + r.unique_users, 0)

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Popular Reports"
        description="Most viewed and downloaded reports in your organization"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Reports', href: '/reports' },
          { label: 'Popular' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
          <Card className="p-6">
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <EyeIcon className="h-8 w-8 text-blue-600" />
              </div>
              <div className="ml-5 w-0 flex-1">
                <dl>
                  <dt className="text-sm font-medium text-gray-500 truncate">Total Views</dt>
                  <dd className="text-lg font-medium text-gray-900">{totalViews.toLocaleString()}</dd>
                </dl>
              </div>
            </div>
          </Card>

          <Card className="p-6">
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <ArrowDownTrayIcon className="h-8 w-8 text-green-600" />
              </div>
              <div className="ml-5 w-0 flex-1">
                <dl>
                  <dt className="text-sm font-medium text-gray-500 truncate">Total Downloads</dt>
                  <dd className="text-lg font-medium text-gray-900">{totalDownloads.toLocaleString()}</dd>
                </dl>
              </div>
            </div>
          </Card>

          <Card className="p-6">
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <StarIcon className="h-8 w-8 text-yellow-600" />
              </div>
              <div className="ml-5 w-0 flex-1">
                <dl>
                  <dt className="text-sm font-medium text-gray-500 truncate">Average Rating</dt>
                  <dd className="text-lg font-medium text-gray-900">{avgRating.toFixed(1)}/5</dd>
                </dl>
              </div>
            </div>
          </Card>

          <Card className="p-6">
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <UsersIcon className="h-8 w-8 text-purple-600" />
              </div>
              <div className="ml-5 w-0 flex-1">
                <dl>
                  <dt className="text-sm font-medium text-gray-500 truncate">Active Users</dt>
                  <dd className="text-lg font-medium text-gray-900">{totalUsers}</dd>
                </dl>
              </div>
            </div>
          </Card>
        </div>

        {/* Filters and Sort */}
        <div className="mb-6 flex flex-col sm:flex-row gap-4">
          <div className="w-full sm:w-48">
            <select
              value={filterCategory}
              onChange={(e) => setFilterCategory(e.target.value)}
              className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
            >
              <option value="all">All Categories</option>
              <option value="financial">Financial</option>
              <option value="sales">Sales</option>
              <option value="purchase">Purchase</option>
              <option value="stock">Stock</option>
              <option value="tax">Tax</option>
            </select>
          </div>
          <div className="w-full sm:w-48">
            <select
              value={sortBy}
              onChange={(e) => setSortBy(e.target.value)}
              className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
            >
              <option value="view_count">Sort by Views</option>
              <option value="download_count">Sort by Downloads</option>
              <option value="unique_users">Sort by Users</option>
              <option value="rating">Sort by Rating</option>
            </select>
          </div>
        </div>

        {/* Top Reports Quick Access */}
        <div className="mb-8">
          <Card>
            <div className="px-6 py-4 border-b border-gray-200">
              <h3 className="text-lg font-medium text-gray-900">Top 3 Reports</h3>
            </div>
            <div className="p-6">
              <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                {filteredReports.slice(0, 3).map((report, index) => (
                  <div key={report.id} className="relative">
                    <div className={`p-4 rounded-lg border-2 ${
                      index === 0 ? 'border-yellow-200 bg-yellow-50' :
                      index === 1 ? 'border-gray-200 bg-gray-50' :
                      'border-orange-200 bg-orange-50'
                    }`}>
                      <div className="flex items-center justify-between mb-2">
                        <span className={`inline-flex items-center justify-center h-8 w-8 rounded-full text-sm font-medium ${
                          index === 0 ? 'bg-yellow-200 text-yellow-800' :
                          index === 1 ? 'bg-gray-200 text-gray-800' :
                          'bg-orange-200 text-orange-800'
                        }`}>
                          #{index + 1}
                        </span>
                        {renderStars(report.rating)}
                      </div>
                      <h4 className="font-medium text-gray-900 mb-1">{report.name}</h4>
                      <p className="text-sm text-gray-600 mb-3">{report.category}</p>
                      <div className="flex items-center justify-between text-xs text-gray-500">
                        <span>{report.view_count} views</span>
                        <span>{report.download_count} downloads</span>
                      </div>
                      <div className="mt-3">
                        <Button size="sm" className="w-full">
                          Generate Now
                        </Button>
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          </Card>
        </div>

        {/* Popular Reports Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <div className="flex items-center justify-between">
              <div>
                <h3 className="text-lg font-medium text-gray-900">Popular Reports Ranking</h3>
                <p className="text-sm text-gray-500">
                  {filteredReports.length} reports ranked by popularity
                </p>
              </div>
              <Button variant="outline" size="sm">
                <CalendarIcon className="h-4 w-4" />
                Export Analytics
              </Button>
            </div>
          </div>
          <Table
            data={filteredReports}
            columns={columns}
            loading={loading}
            emptyMessage="No popular reports found"
          />
        </Card>
      </main>
    </div>
  )
}