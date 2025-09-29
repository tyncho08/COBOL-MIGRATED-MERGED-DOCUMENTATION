'use client'

import { useState, useEffect } from 'react'
import { 
  CalendarIcon,
  LockClosedIcon,
  LockOpenIcon,
  CheckCircleIcon,
  ExclamationTriangleIcon,
  ClockIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import Table from '@/components/UI/Table'
import { formatCurrency } from '@/lib/utils'

interface GLPeriod {
  period_key: string
  period_name: string
  start_date: string
  end_date: string
  status: 'open' | 'closed' | 'archived'
  fiscal_year: number
  period_number: number
  transaction_count: number
  total_debits: number
  total_credits: number
  variance: number
  closed_by?: string
  closed_date?: string
}

export default function GLPeriodsPage() {
  const [periods, setPeriods] = useState<GLPeriod[]>([])
  const [loading, setLoading] = useState(true)
  const [currentPeriod, setCurrentPeriod] = useState<GLPeriod | null>(null)

  useEffect(() => {
    const fetchPeriods = async () => {
      try {
        const response = await fetch('http://localhost:8000/api/v1/gl/periods')
        if (response.ok) {
          const data = await response.json()
          setPeriods(data.periods || [])
          setCurrentPeriod(data.current_period)
        } else {
          // Fallback data
          const fallbackPeriods = [
            {
              period_key: '202501',
              period_name: 'January 2025',
              start_date: '2025-01-01',
              end_date: '2025-01-31',
              status: 'open' as const,
              fiscal_year: 2025,
              period_number: 1,
              transaction_count: 45,
              total_debits: 125000.00,
              total_credits: 125000.00,
              variance: 0.00
            },
            {
              period_key: '202412',
              period_name: 'December 2024',
              start_date: '2024-12-01',
              end_date: '2024-12-31',
              status: 'closed' as const,
              fiscal_year: 2024,
              period_number: 12,
              transaction_count: 89,
              total_debits: 234000.00,
              total_credits: 234000.00,
              variance: 0.00,
              closed_by: 'ADMIN',
              closed_date: '2025-01-05'
            }
          ]
          setPeriods(fallbackPeriods)
          setCurrentPeriod(fallbackPeriods[0])
        }
      } catch (error) {
        console.error('Failed to fetch periods:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchPeriods()
  }, [])

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        <CalendarIcon className="h-4 w-4" />
        New Period
      </Button>
      <Button size="sm" disabled={!currentPeriod || currentPeriod.status !== 'open'}>
        <LockClosedIcon className="h-4 w-4" />
        Close Current Period
      </Button>
    </div>
  )

  const getStatusBadge = (status: string) => {
    switch (status) {
      case 'open':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
            <LockOpenIcon className="w-3 h-3 mr-1" />
            Open
          </span>
        )
      case 'closed':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
            <LockClosedIcon className="w-3 h-3 mr-1" />
            Closed
          </span>
        )
      case 'archived':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
            <CheckCircleIcon className="w-3 h-3 mr-1" />
            Archived
          </span>
        )
      default:
        return null
    }
  }

  const columns = [
    {
      key: 'period_name',
      header: 'Period',
      className: 'min-w-[150px]',
      render: (value: any, row: GLPeriod) => (
        <div>
          <div className="font-medium text-gray-900">{value}</div>
          <div className="text-sm text-gray-500">
            {new Date(row.start_date).toLocaleDateString()} - {new Date(row.end_date).toLocaleDateString()}
          </div>
        </div>
      )
    },
    {
      key: 'status',
      header: 'Status',
      className: 'w-24',
      render: (value: any) => getStatusBadge(value)
    },
    {
      key: 'transaction_count',
      header: 'Transactions',
      className: 'w-32 text-center',
      render: (value: any) => (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
          {value}
        </span>
      )
    },
    {
      key: 'total_debits',
      header: 'Total Debits',
      className: 'w-32 text-right',
      render: (value: any) => (
        <div className="text-right font-mono">
          {formatCurrency(value)}
        </div>
      )
    },
    {
      key: 'total_credits',
      header: 'Total Credits',
      className: 'w-32 text-right',
      render: (value: any) => (
        <div className="text-right font-mono">
          {formatCurrency(value)}
        </div>
      )
    },
    {
      key: 'variance',
      header: 'Variance',
      className: 'w-32 text-right',
      render: (value: any) => (
        <div className={`text-right font-mono ${value === 0 ? 'text-green-600' : 'text-red-600'}`}>
          {value === 0 ? 'Balanced' : formatCurrency(Math.abs(value))}
        </div>
      )
    },
    {
      key: 'actions',
      header: 'Actions',
      className: 'w-32',
      render: (value: any, row: GLPeriod) => (
        <div className="flex space-x-1">
          {row.status === 'open' && (
            <Button variant="outline" size="xs">
              Close
            </Button>
          )}
          <Button variant="outline" size="xs">
            View
          </Button>
        </div>
      )
    }
  ]

  const openPeriods = periods.filter(p => p.status === 'open').length
  const totalTransactions = periods.reduce((sum, p) => sum + p.transaction_count, 0)

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="GL Periods"
        description="Manage accounting periods and period-end processing"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'General Ledger', href: '/gl' },
          { label: 'Periods' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
          <StatsCard
            title="Current Period"
            value={currentPeriod?.period_name || 'None'}
            icon={<CalendarIcon className="h-6 w-6" />}
            change={{ 
              value: currentPeriod?.status === 'open' ? 'Open for posting' : 'Closed', 
              type: currentPeriod?.status === 'open' ? 'increase' : 'neutral' 
            }}
          />
          <StatsCard
            title="Open Periods"
            value={openPeriods.toString()}
            icon={<LockOpenIcon className="h-6 w-6" />}
            change={{ 
              value: `${periods.length - openPeriods} closed`, 
              type: 'neutral' 
            }}
          />
          <StatsCard
            title="Total Transactions"
            value={totalTransactions.toString()}
            icon={<ClockIcon className="h-6 w-6" />}
            change={{ 
              value: 'All periods', 
              type: 'neutral' 
            }}
          />
          <StatsCard
            title="Period Status"
            value={currentPeriod?.variance === 0 ? 'Balanced' : 'Out of Balance'}
            icon={currentPeriod?.variance === 0 ? 
              <CheckCircleIcon className="h-6 w-6" /> : 
              <ExclamationTriangleIcon className="h-6 w-6" />
            }
            change={{ 
              value: currentPeriod ? formatCurrency(Math.abs(currentPeriod.variance)) : '£0.00', 
              type: currentPeriod?.variance === 0 ? 'neutral' : 'decrease' 
            }}
          />
        </div>

        {/* Current Period Alert */}
        {currentPeriod && currentPeriod.variance !== 0 && (
          <div className="mb-8">
            <Card>
              <div className="p-6">
                <div className="rounded-md bg-yellow-50 p-4">
                  <div className="flex">
                    <div className="flex-shrink-0">
                      <ExclamationTriangleIcon className="h-5 w-5 text-yellow-400" />
                    </div>
                    <div className="ml-3">
                      <h3 className="text-sm font-medium text-yellow-800">
                        Period Out of Balance
                      </h3>
                      <div className="mt-2 text-sm text-yellow-700">
                        <p>
                          The current period has a variance of {formatCurrency(Math.abs(currentPeriod.variance))}.
                          Please review transactions before closing the period.
                        </p>
                      </div>
                      <div className="mt-4">
                        <Button size="sm" variant="outline">
                          Review Transactions
                        </Button>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </Card>
          </div>
        )}

        {/* Periods Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <div className="flex items-center justify-between">
              <div>
                <h3 className="text-lg font-medium text-gray-900">Accounting Periods</h3>
                <p className="text-sm text-gray-500">
                  {periods.length} periods across fiscal years
                </p>
              </div>
              <Button variant="outline" size="sm">
                <CalendarIcon className="h-4 w-4" />
                Period Settings
              </Button>
            </div>
          </div>
          <Table
            data={periods}
            columns={columns}
            loading={loading}
            emptyMessage="No periods found"
          />
        </Card>
      </main>
    </div>
  )
}