'use client'

import { useState, useEffect } from 'react'
import { 
  ExclamationTriangleIcon,
  ClockIcon,
  CheckCircleIcon,
  XMarkIcon,
  EyeIcon,
  DocumentTextIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import Table from '@/components/UI/Table'
import { formatCurrency, formatDate } from '@/lib/utils'

interface PendingItem {
  entry_id: number
  entry_type: string
  reference: string
  description: string
  amount: number
  debit_credit: string
  status: string
  priority: string
  created_by: string
  assigned_to?: string
  created_date: string
  due_date?: string
  days_pending: number
}

export default function PendingItemsPage() {
  const [pendingItems, setPendingItems] = useState<PendingItem[]>([])
  const [loading, setLoading] = useState(true)
  const [filterType, setFilterType] = useState('all')
  const [filterPriority, setFilterPriority] = useState('all')

  useEffect(() => {
    const fetchPendingItems = async () => {
      try {
        const response = await fetch('http://localhost:8000/api/v1/gl/pending')
        if (response.ok) {
          const data = await response.json()
          setPendingItems(data.entries || [])
        }
      } catch (error) {
        console.error('Failed to fetch pending items:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchPendingItems()
  }, [])

  const filteredItems = pendingItems.filter(item => {
    const matchesType = filterType === 'all' || item.entry_type?.toLowerCase() === filterType
    const matchesPriority = filterPriority === 'all' || item.priority?.toLowerCase() === filterPriority
    return matchesType && matchesPriority
  })

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        <DocumentTextIcon className="h-4 w-4" />
        New Journal
      </Button>
      <Button size="sm">
        <CheckCircleIcon className="h-4 w-4" />
        Bulk Approve
      </Button>
    </div>
  )

  const getTypeBadge = (type: string) => {
    const badges = {
      JOURNAL: { label: 'Journal Entry', class: 'bg-blue-100 text-blue-800' },
      APPROVAL: { label: 'Approval', class: 'bg-orange-100 text-orange-800' },
      RECONCILIATION: { label: 'Reconciliation', class: 'bg-purple-100 text-purple-800' },
      REVIEW: { label: 'Review', class: 'bg-gray-100 text-gray-800' }
    }
    const badge = badges[type as keyof typeof badges] || badges.REVIEW
    return (
      <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${badge.class}`}>
        {badge.label}
      </span>
    )
  }

  const getPriorityBadge = (priority: string) => {
    switch (priority?.toLowerCase()) {
      case 'high':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
            <ExclamationTriangleIcon className="w-3 h-3 mr-1" />
            High
          </span>
        )
      case 'medium':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-yellow-100 text-yellow-800">
            <ClockIcon className="w-3 h-3 mr-1" />
            Medium
          </span>
        )
      case 'low':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
            Low
          </span>
        )
      default:
        return null
    }
  }

  const getStatusBadge = (status: string) => {
    switch (status?.toLowerCase()) {
      case 'pending':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-yellow-100 text-yellow-800">
            <ClockIcon className="w-3 h-3 mr-1" />
            Pending
          </span>
        )
      case 'in_review':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
            <EyeIcon className="w-3 h-3 mr-1" />
            In Review
          </span>
        )
      case 'rejected':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
            <XMarkIcon className="w-3 h-3 mr-1" />
            Rejected
          </span>
        )
      default:
        return null
    }
  }

  const columns = [
    {
      key: 'reference',
      header: 'Reference',
      className: 'min-w-[150px]',
      render: (value: any, row: PendingItem) => (
        <div>
          <div className="font-medium text-gray-900">{value}</div>
          <div className="text-sm text-gray-500">{row.description}</div>
        </div>
      )
    },
    {
      key: 'entry_type',
      header: 'Type',
      className: 'w-32',
      render: (value: any) => getTypeBadge(value)
    },
    {
      key: 'priority',
      header: 'Priority',
      className: 'w-24',
      render: (value: any) => getPriorityBadge(value)
    },
    {
      key: 'status',
      header: 'Status',
      className: 'w-24',
      render: (value: any) => getStatusBadge(value)
    },
    {
      key: 'amount',
      header: 'Amount',
      className: 'w-32 text-right',
      render: (value: any) => (
        <div className="text-right font-mono">
          {formatCurrency(value)}
        </div>
      )
    },
    {
      key: 'created_by',
      header: 'Created By',
      className: 'w-32',
      render: (value: any, row: PendingItem) => (
        <div>
          <div className="text-sm font-medium text-gray-900">{value}</div>
          <div className="text-xs text-gray-500">{formatDate(row.created_date)}</div>
        </div>
      )
    },
    {
      key: 'assigned_to',
      header: 'Assigned To',
      className: 'w-32',
      render: (value: any, row: PendingItem) => (
        <div>
          {value ? (
            <span className="text-sm text-gray-900">{value}</span>
          ) : (
            <span className="text-sm text-gray-400">Unassigned</span>
          )}
          {row.due_date && (
            <div className="text-xs text-gray-500">
              Due: {formatDate(row.due_date)}
            </div>
          )}
        </div>
      )
    },
    {
      key: 'actions',
      header: 'Actions',
      className: 'w-32',
      render: (value: any, row: PendingItem) => (
        <div className="flex space-x-1">
          <Button variant="outline" size="xs">
            <EyeIcon className="h-3 w-3" />
          </Button>
          <Button variant="outline" size="xs">
            <CheckCircleIcon className="h-3 w-3" />
          </Button>
          <Button variant="outline" size="xs">
            <XMarkIcon className="h-3 w-3" />
          </Button>
        </div>
      )
    }
  ]

  const highPriorityCount = pendingItems.filter(item => item.priority?.toLowerCase() === 'high').length
  const overdueCount = pendingItems.filter(item => 
    item.due_date && new Date(item.due_date) < new Date()
  ).length
  const totalAmount = pendingItems.reduce((sum, item) => sum + item.amount, 0)

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Pending Items"
        description="Review and approve pending journal entries and transactions"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'General Ledger', href: '/gl' },
          { label: 'Pending Items' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
          <StatsCard
            title="Total Pending"
            value={pendingItems.length.toString()}
            icon={<ClockIcon className="h-6 w-6" />}
            change={{ 
              value: `${filteredItems.length} shown`, 
              type: 'neutral' 
            }}
          />
          <StatsCard
            title="High Priority"
            value={highPriorityCount.toString()}
            icon={<ExclamationTriangleIcon className="h-6 w-6" />}
            change={{ 
              value: 'Needs attention', 
              type: highPriorityCount > 0 ? 'decrease' : 'neutral' 
            }}
          />
          <StatsCard
            title="Overdue Items"
            value={overdueCount.toString()}
            icon={<ExclamationTriangleIcon className="h-6 w-6" />}
            change={{ 
              value: 'Past due date', 
              type: overdueCount > 0 ? 'decrease' : 'neutral' 
            }}
          />
          <StatsCard
            title="Total Amount"
            value={formatCurrency(totalAmount)}
            icon={<DocumentTextIcon className="h-6 w-6" />}
            change={{ 
              value: 'Pending approval', 
              type: 'neutral' 
            }}
          />
        </div>

        {/* Filters */}
        <div className="mb-6 flex flex-col sm:flex-row gap-4">
          <div className="w-full sm:w-48">
            <select
              value={filterType}
              onChange={(e) => setFilterType(e.target.value)}
              className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
            >
              <option value="all">All Types</option>
              <option value="journal">Journal Entries</option>
              <option value="approval">Approvals</option>
              <option value="reconciliation">Reconciliations</option>
              <option value="review">Reviews</option>
            </select>
          </div>
          <div className="w-full sm:w-48">
            <select
              value={filterPriority}
              onChange={(e) => setFilterPriority(e.target.value)}
              className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
            >
              <option value="all">All Priorities</option>
              <option value="high">High Priority</option>
              <option value="medium">Medium Priority</option>
              <option value="low">Low Priority</option>
            </select>
          </div>
        </div>

        {/* High Priority Alert */}
        {highPriorityCount > 0 && (
          <div className="mb-8">
            <Card>
              <div className="p-6">
                <div className="rounded-md bg-red-50 p-4">
                  <div className="flex">
                    <div className="flex-shrink-0">
                      <ExclamationTriangleIcon className="h-5 w-5 text-red-400" />
                    </div>
                    <div className="ml-3">
                      <h3 className="text-sm font-medium text-red-800">
                        High Priority Items Pending
                      </h3>
                      <div className="mt-2 text-sm text-red-700">
                        <p>
                          You have {highPriorityCount} high priority item{highPriorityCount !== 1 ? 's' : ''} 
                          that require{highPriorityCount === 1 ? 's' : ''} immediate attention.
                        </p>
                      </div>
                      <div className="mt-4">
                        <Button size="sm" variant="outline" onClick={() => setFilterPriority('high')}>
                          View High Priority Items
                        </Button>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </Card>
          </div>
        )}

        {/* Pending Items Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <div className="flex items-center justify-between">
              <div>
                <h3 className="text-lg font-medium text-gray-900">Pending Items</h3>
                <p className="text-sm text-gray-500">
                  {filteredItems.length} of {pendingItems.length} items
                </p>
              </div>
              <Button variant="outline" size="sm">
                <CheckCircleIcon className="h-4 w-4" />
                Bulk Actions
              </Button>
            </div>
          </div>
          <Table
            data={filteredItems}
            columns={columns}
            loading={loading}
            emptyMessage="No pending items found"
          />
        </Card>
      </main>
    </div>
  )
}