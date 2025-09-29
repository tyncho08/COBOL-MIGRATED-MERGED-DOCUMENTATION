'use client'

import { useState, useEffect } from 'react'
import { 
  DocumentTextIcon,
  MagnifyingGlassIcon,
  FunnelIcon,
  ArrowDownTrayIcon,
  PrinterIcon,
  CalendarIcon,
  CurrencyDollarIcon,
  UsersIcon,
  TruckIcon,
  CubeIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import Input from '@/components/UI/Input'
import Table from '@/components/UI/Table'

interface Report {
  id: string
  name: string
  description: string
  category: string
  last_generated?: string
  frequency: string
  format: string[]
  size?: string
  status: 'available' | 'generating' | 'error'
}

export default function AllReportsPage() {
  const [reports, setReports] = useState<Report[]>([])
  const [loading, setLoading] = useState(true)
  const [searchTerm, setSearchTerm] = useState('')
  const [filterCategory, setFilterCategory] = useState('all')
  const [filterStatus, setFilterStatus] = useState('all')

  useEffect(() => {
    const fetchReports = async () => {
      try {
        const response = await fetch('http://localhost:8000/api/v1/reports/all')
        if (response.ok) {
          const data = await response.json()
          setReports(data.reports || [])
        } else {
          // Fallback data
          setReports([
            {
              id: 'trial-balance',
              name: 'Trial Balance',
              description: 'Complete trial balance with all GL accounts',
              category: 'Financial',
              last_generated: '2025-01-15T09:30:00Z',
              frequency: 'Daily',
              format: ['PDF', 'Excel', 'CSV'],
              size: '1.2 MB',
              status: 'available'
            },
            {
              id: 'profit-loss',
              name: 'Profit & Loss Statement',
              description: 'Income statement showing revenue and expenses',
              category: 'Financial',
              last_generated: '2025-01-15T08:45:00Z',
              frequency: 'Monthly',
              format: ['PDF', 'Excel'],
              size: '894 KB',
              status: 'available'
            },
            {
              id: 'balance-sheet',
              name: 'Balance Sheet',
              description: 'Statement of financial position',
              category: 'Financial',
              last_generated: '2025-01-14T17:20:00Z',
              frequency: 'Monthly',
              format: ['PDF', 'Excel'],
              size: '756 KB',
              status: 'available'
            },
            {
              id: 'customer-aging',
              name: 'Customer Aging Report',
              description: 'Outstanding receivables by aging buckets',
              category: 'Sales',
              last_generated: '2025-01-15T10:15:00Z',
              frequency: 'Weekly',
              format: ['PDF', 'Excel', 'CSV'],
              size: '2.1 MB',
              status: 'available'
            },
            {
              id: 'sales-analysis',
              name: 'Sales Analysis',
              description: 'Sales performance by customer, product, and territory',
              category: 'Sales',
              last_generated: '2025-01-15T07:30:00Z',
              frequency: 'Monthly',
              format: ['PDF', 'Excel'],
              size: '3.4 MB',
              status: 'available'
            },
            {
              id: 'supplier-aging',
              name: 'Supplier Aging Report',
              description: 'Outstanding payables by aging buckets',
              category: 'Purchase',
              last_generated: '2025-01-15T09:45:00Z',
              frequency: 'Weekly',
              format: ['PDF', 'Excel', 'CSV'],
              size: '1.8 MB',
              status: 'available'
            },
            {
              id: 'stock-valuation',
              name: 'Stock Valuation Report',
              description: 'Inventory valuation by location and category',
              category: 'Stock',
              last_generated: '2025-01-15T08:00:00Z',
              frequency: 'Daily',
              format: ['PDF', 'Excel', 'CSV'],
              size: '4.2 MB',
              status: 'available'
            }
          ])
        }
      } catch (error) {
        console.error('Failed to fetch all reports:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchReports()
  }, [])

  const filteredReports = reports.filter(report => {
    const matchesSearch = report.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
                         report.description.toLowerCase().includes(searchTerm.toLowerCase())
    const matchesCategory = filterCategory === 'all' || report.category.toLowerCase() === filterCategory.toLowerCase()
    const matchesStatus = filterStatus === 'all' || report.status === filterStatus
    return matchesSearch && matchesCategory && matchesStatus
  })

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        <FunnelIcon className="h-4 w-4" />
        Advanced Filter
      </Button>
      <Button size="sm">
        <DocumentTextIcon className="h-4 w-4" />
        Generate Report
      </Button>
    </div>
  )

  const getCategoryIcon = (category: string) => {
    const iconMap: Record<string, any> = {
      'Financial': CurrencyDollarIcon,
      'Sales': UsersIcon,
      'Purchase': TruckIcon,
      'Stock': CubeIcon
    }
    const IconComponent = iconMap[category] || DocumentTextIcon
    return <IconComponent className="h-4 w-4" />
  }

  const getStatusBadge = (status: string) => {
    switch (status) {
      case 'available':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
            Available
          </span>
        )
      case 'generating':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-yellow-100 text-yellow-800">
            Generating
          </span>
        )
      case 'error':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
            Error
          </span>
        )
      default:
        return null
    }
  }

  const columns = [
    {
      key: 'name',
      header: 'Report Name',
      className: 'min-w-[200px]',
      render: (value: any, row: Report) => (
        <div className="flex items-center">
          <div className="flex-shrink-0 mr-3">
            <div className="p-2 rounded-md bg-indigo-50">
              {getCategoryIcon(row.category)}
            </div>
          </div>
          <div>
            <div className="font-medium text-gray-900">{value}</div>
            <div className="text-sm text-gray-500">{row.description}</div>
          </div>
        </div>
      )
    },
    {
      key: 'category',
      header: 'Category',
      className: 'w-24',
      render: (value: any) => (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
          {value}
        </span>
      )
    },
    {
      key: 'frequency',
      header: 'Frequency',
      className: 'w-24',
      render: (value: any) => (
        <span className="text-sm text-gray-900">{value}</span>
      )
    },
    {
      key: 'last_generated',
      header: 'Last Generated',
      className: 'w-32',
      render: (value: any) => (
        <div className="text-sm text-gray-900">
          {value ? new Date(value).toLocaleDateString() : 'Never'}
        </div>
      )
    },
    {
      key: 'size',
      header: 'Size',
      className: 'w-24',
      render: (value: any) => (
        <span className="text-sm text-gray-500">{value || 'N/A'}</span>
      )
    },
    {
      key: 'status',
      header: 'Status',
      className: 'w-24',
      render: (value: any) => getStatusBadge(value)
    },
    {
      key: 'actions',
      header: 'Actions',
      className: 'w-32',
      render: (value: any, row: Report) => (
        <div className="flex space-x-1">
          <Button variant="outline" size="xs" disabled={row.status !== 'available'}>
            <ArrowDownTrayIcon className="h-3 w-3" />
          </Button>
          <Button variant="outline" size="xs">
            <PrinterIcon className="h-3 w-3" />
          </Button>
        </div>
      )
    }
  ]

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="All Reports"
        description="Browse and manage all available reports"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Reports', href: '/reports' },
          { label: 'All Reports' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Search and Filters */}
        <div className="mb-6 flex flex-col sm:flex-row gap-4">
          <div className="flex-1">
            <div className="relative">
              <MagnifyingGlassIcon className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
              <Input
                type="text"
                placeholder="Search reports..."
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                className="pl-10"
              />
            </div>
          </div>
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
            </select>
          </div>
          <div className="w-full sm:w-48">
            <select
              value={filterStatus}
              onChange={(e) => setFilterStatus(e.target.value)}
              className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
            >
              <option value="all">All Status</option>
              <option value="available">Available</option>
              <option value="generating">Generating</option>
              <option value="error">Error</option>
            </select>
          </div>
        </div>

        {/* Reports Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <div className="flex items-center justify-between">
              <div>
                <h3 className="text-lg font-medium text-gray-900">Available Reports</h3>
                <p className="text-sm text-gray-500">
                  {filteredReports.length} of {reports.length} reports
                </p>
              </div>
              <Button variant="outline" size="sm">
                <CalendarIcon className="h-4 w-4" />
                Schedule Reports
              </Button>
            </div>
          </div>
          <Table
            data={filteredReports}
            columns={columns}
            loading={loading}
            emptyMessage="No reports found matching your criteria"
          />
        </Card>
      </main>
    </div>
  )
}