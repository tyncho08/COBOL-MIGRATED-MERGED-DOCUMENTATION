'use client'

import { useState, useEffect } from 'react'
import { 
  ClockIcon,
  ArrowDownTrayIcon,
  EyeIcon,
  TrashIcon,
  MagnifyingGlassIcon,
  CalendarIcon,
  DocumentTextIcon,
  FunnelIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import Input from '@/components/UI/Input'
import Table from '@/components/UI/Table'

interface ReportHistoryItem {
  id: string
  report_name: string
  report_type: string
  generated_date: string
  generated_by: string
  parameters: string
  format: string
  file_size: string
  download_count: number
  status: 'available' | 'archived' | 'expired'
  expiry_date?: string
}

export default function ReportHistoryPage() {
  const [reportHistory, setReportHistory] = useState<ReportHistoryItem[]>([])
  const [loading, setLoading] = useState(true)
  const [searchTerm, setSearchTerm] = useState('')
  const [filterPeriod, setFilterPeriod] = useState('all')
  const [filterUser, setFilterUser] = useState('all')

  useEffect(() => {
    const fetchReportHistory = async () => {
      try {
        const response = await fetch('http://localhost:8000/api/v1/reports/history')
        if (response.ok) {
          const data = await response.json()
          setReportHistory(data.history || [])
        } else {
          // Fallback data
          setReportHistory([
            {
              id: 'hist-001',
              report_name: 'Trial Balance',
              report_type: 'Financial',
              generated_date: '2025-01-15T09:30:00Z',
              generated_by: 'ACCOUNTANT',
              parameters: 'Period: Jan 2025, All Accounts',
              format: 'PDF',
              file_size: '1.2 MB',
              download_count: 3,
              status: 'available',
              expiry_date: '2025-04-15T00:00:00Z'
            },
            {
              id: 'hist-002',
              report_name: 'Customer Aging Report',
              report_type: 'Sales',
              generated_date: '2025-01-15T08:45:00Z',
              generated_by: 'SALES_MANAGER',
              parameters: 'All Customers, 30-60-90 Days',
              format: 'Excel',
              file_size: '2.1 MB',
              download_count: 7,
              status: 'available',
              expiry_date: '2025-04-15T00:00:00Z'
            },
            {
              id: 'hist-003',
              report_name: 'Stock Valuation Report',
              report_type: 'Stock',
              generated_date: '2025-01-14T16:20:00Z',
              generated_by: 'WAREHOUSE_MANAGER',
              parameters: 'All Locations, FIFO Method',
              format: 'PDF',
              file_size: '4.2 MB',
              download_count: 2,
              status: 'available',
              expiry_date: '2025-04-14T00:00:00Z'
            },
            {
              id: 'hist-004',
              report_name: 'P&L Statement',
              report_type: 'Financial',
              generated_date: '2025-01-13T10:15:00Z',
              generated_by: 'CFO',
              parameters: 'Dec 2024, Consolidated',
              format: 'PDF',
              file_size: '894 KB',
              download_count: 12,
              status: 'available',
              expiry_date: '2025-04-13T00:00:00Z'
            },
            {
              id: 'hist-005',
              report_name: 'VAT Return',
              report_type: 'Tax',
              generated_date: '2025-01-10T14:30:00Z',
              generated_by: 'ACCOUNTANT',
              parameters: 'Q4 2024, Standard Rate',
              format: 'PDF',
              file_size: '567 KB',
              download_count: 1,
              status: 'archived',
              expiry_date: '2025-04-10T00:00:00Z'
            },
            {
              id: 'hist-006',
              report_name: 'Monthly Sales Analysis',
              report_type: 'Sales',
              generated_date: '2025-01-05T09:00:00Z',
              generated_by: 'SALES_MANAGER',
              parameters: 'Dec 2024, By Territory',
              format: 'Excel',
              file_size: '3.4 MB',
              download_count: 5,
              status: 'expired'
            }
          ])
        }
      } catch (error) {
        console.error('Failed to fetch report history:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchReportHistory()
  }, [])

  const filteredHistory = reportHistory.filter(item => {
    const matchesSearch = item.report_name.toLowerCase().includes(searchTerm.toLowerCase()) ||
                         item.generated_by.toLowerCase().includes(searchTerm.toLowerCase())
    
    const matchesPeriod = filterPeriod === 'all' || 
      (filterPeriod === 'today' && new Date(item.generated_date).toDateString() === new Date().toDateString()) ||
      (filterPeriod === 'week' && new Date(item.generated_date) > new Date(Date.now() - 7 * 24 * 60 * 60 * 1000)) ||
      (filterPeriod === 'month' && new Date(item.generated_date) > new Date(Date.now() - 30 * 24 * 60 * 60 * 1000))
    
    const matchesUser = filterUser === 'all' || item.generated_by === filterUser

    return matchesSearch && matchesPeriod && matchesUser
  })

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        <FunnelIcon className="h-4 w-4" />
        Advanced Filter
      </Button>
      <Button variant="outline" size="sm">
        <TrashIcon className="h-4 w-4" />
        Cleanup Old Reports
      </Button>
    </div>
  )

  const getStatusBadge = (status: string) => {
    switch (status) {
      case 'available':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
            Available
          </span>
        )
      case 'archived':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
            Archived
          </span>
        )
      case 'expired':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
            Expired
          </span>
        )
      default:
        return null
    }
  }

  const getFormatBadge = (format: string) => {
    const colorMap: Record<string, string> = {
      'PDF': 'bg-red-100 text-red-800',
      'Excel': 'bg-green-100 text-green-800',
      'CSV': 'bg-blue-100 text-blue-800'
    }
    
    return (
      <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${colorMap[format] || 'bg-gray-100 text-gray-800'}`}>
        {format}
      </span>
    )
  }

  const columns = [
    {
      key: 'report_name',
      header: 'Report Name',
      className: 'min-w-[200px]',
      render: (value: any, row: ReportHistoryItem) => (
        <div>
          <div className="font-medium text-gray-900">{value}</div>
          <div className="text-sm text-gray-500">{row.parameters}</div>
        </div>
      )
    },
    {
      key: 'report_type',
      header: 'Type',
      className: 'w-24',
      render: (value: any) => (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-indigo-100 text-indigo-800">
          {value}
        </span>
      )
    },
    {
      key: 'generated_date',
      header: 'Generated',
      className: 'w-32',
      render: (value: any, row: ReportHistoryItem) => (
        <div>
          <div className="text-sm text-gray-900">
            {new Date(value).toLocaleDateString()}
          </div>
          <div className="text-xs text-gray-500">
            {new Date(value).toLocaleTimeString()}
          </div>
        </div>
      )
    },
    {
      key: 'generated_by',
      header: 'Generated By',
      className: 'w-32',
      render: (value: any) => (
        <span className="text-sm text-gray-900">{value}</span>
      )
    },
    {
      key: 'format',
      header: 'Format',
      className: 'w-20',
      render: (value: any) => getFormatBadge(value)
    },
    {
      key: 'file_size',
      header: 'Size',
      className: 'w-24',
      render: (value: any) => (
        <span className="text-sm text-gray-500">{value}</span>
      )
    },
    {
      key: 'download_count',
      header: 'Downloads',
      className: 'w-24 text-center',
      render: (value: any) => (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
          {value}
        </span>
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
      render: (value: any, row: ReportHistoryItem) => (
        <div className="flex space-x-1">
          <Button variant="outline" size="xs" disabled={row.status === 'expired'}>
            <ArrowDownTrayIcon className="h-3 w-3" />
          </Button>
          <Button variant="outline" size="xs">
            <EyeIcon className="h-3 w-3" />
          </Button>
          <Button variant="outline" size="xs">
            <TrashIcon className="h-3 w-3" />
          </Button>
        </div>
      )
    }
  ]

  const availableReports = reportHistory.filter(r => r.status === 'available').length
  const totalDownloads = reportHistory.reduce((sum, r) => sum + r.download_count, 0)
  const totalSize = reportHistory.reduce((sum, r) => {
    // Ensure file_size is a string and handle undefined/null cases
    const fileSize = r.file_size?.toString() || '0 MB'
    const sizeInMB = parseFloat(fileSize.replace(/[^\d.]/g, '')) || 0
    return sum + (fileSize.includes('KB') ? sizeInMB / 1024 : sizeInMB)
  }, 0)

  const uniqueUsers = Array.from(new Set(reportHistory.map(r => r.generated_by)))

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Report History"
        description="View and manage previously generated reports"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Reports', href: '/reports' },
          { label: 'History' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
          <Card className="p-6">
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <DocumentTextIcon className="h-8 w-8 text-indigo-600" />
              </div>
              <div className="ml-5 w-0 flex-1">
                <dl>
                  <dt className="text-sm font-medium text-gray-500 truncate">Total Reports</dt>
                  <dd className="text-lg font-medium text-gray-900">{reportHistory.length}</dd>
                </dl>
              </div>
            </div>
          </Card>

          <Card className="p-6">
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <ClockIcon className="h-8 w-8 text-green-600" />
              </div>
              <div className="ml-5 w-0 flex-1">
                <dl>
                  <dt className="text-sm font-medium text-gray-500 truncate">Available</dt>
                  <dd className="text-lg font-medium text-gray-900">{availableReports}</dd>
                </dl>
              </div>
            </div>
          </Card>

          <Card className="p-6">
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <ArrowDownTrayIcon className="h-8 w-8 text-blue-600" />
              </div>
              <div className="ml-5 w-0 flex-1">
                <dl>
                  <dt className="text-sm font-medium text-gray-500 truncate">Total Downloads</dt>
                  <dd className="text-lg font-medium text-gray-900">{totalDownloads}</dd>
                </dl>
              </div>
            </div>
          </Card>

          <Card className="p-6">
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <DocumentTextIcon className="h-8 w-8 text-purple-600" />
              </div>
              <div className="ml-5 w-0 flex-1">
                <dl>
                  <dt className="text-sm font-medium text-gray-500 truncate">Storage Used</dt>
                  <dd className="text-lg font-medium text-gray-900">{totalSize.toFixed(1)} MB</dd>
                </dl>
              </div>
            </div>
          </Card>
        </div>

        {/* Search and Filters */}
        <div className="mb-6 flex flex-col sm:flex-row gap-4">
          <div className="flex-1">
            <div className="relative">
              <MagnifyingGlassIcon className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
              <Input
                type="text"
                placeholder="Search reports or users..."
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                className="pl-10"
              />
            </div>
          </div>
          <div className="w-full sm:w-48">
            <select
              value={filterPeriod}
              onChange={(e) => setFilterPeriod(e.target.value)}
              className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
            >
              <option value="all">All Time</option>
              <option value="today">Today</option>
              <option value="week">This Week</option>
              <option value="month">This Month</option>
            </select>
          </div>
          <div className="w-full sm:w-48">
            <select
              value={filterUser}
              onChange={(e) => setFilterUser(e.target.value)}
              className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
            >
              <option value="all">All Users</option>
              {uniqueUsers.map(user => (
                <option key={user} value={user}>{user}</option>
              ))}
            </select>
          </div>
        </div>

        {/* Report History Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <div className="flex items-center justify-between">
              <div>
                <h3 className="text-lg font-medium text-gray-900">Report History</h3>
                <p className="text-sm text-gray-500">
                  {filteredHistory.length} of {reportHistory.length} reports
                </p>
              </div>
              <Button variant="outline" size="sm">
                <CalendarIcon className="h-4 w-4" />
                Export History
              </Button>
            </div>
          </div>
          <Table
            data={filteredHistory}
            columns={columns}
            loading={loading}
            emptyMessage="No report history found"
          />
        </Card>
      </main>
    </div>
  )
}