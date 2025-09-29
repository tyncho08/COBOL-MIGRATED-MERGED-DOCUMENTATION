'use client'

import { useState, useEffect } from 'react'
import { 
  ScaleIcon,
  PrinterIcon,
  ArrowDownTrayIcon,
  CalendarIcon,
  CheckCircleIcon,
  ExclamationTriangleIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import Table from '@/components/UI/Table'
import { formatCurrency } from '@/lib/utils'

interface TrialBalanceItem {
  account_code: string
  account_name: string
  debit_balance: number
  credit_balance: number
  account_type: string
}

interface TrialBalanceSummary {
  total_debits: number
  total_credits: number
  variance: number
  is_balanced: boolean
  period: string
  as_at_date: string
}

export default function TrialBalancePage() {
  const [trialBalance, setTrialBalance] = useState<TrialBalanceItem[]>([])
  const [summary, setSummary] = useState<TrialBalanceSummary | null>(null)
  const [loading, setLoading] = useState(true)
  const [selectedPeriod, setSelectedPeriod] = useState('current')

  useEffect(() => {
    const fetchTrialBalance = async () => {
      try {
        const response = await fetch(`http://localhost:8000/api/v1/gl/trial-balance?period=${selectedPeriod}`)
        if (response.ok) {
          const data = await response.json()
          setTrialBalance(data.items || [])
          setSummary(data.summary)
        } else {
          // Fallback data
          const fallbackData = [
            {
              account_code: '10010000',
              account_name: 'Petty Cash',
              debit_balance: 500.00,
              credit_balance: 0.00,
              account_type: 'Asset'
            },
            {
              account_code: '10020000',
              account_name: 'Bank Current Account',
              debit_balance: 25000.00,
              credit_balance: 0.00,
              account_type: 'Asset'
            },
            {
              account_code: '40010000',
              account_name: 'Sales Revenue',
              debit_balance: 0.00,
              credit_balance: 82000.00,
              account_type: 'Income'
            }
          ]
          setTrialBalance(fallbackData)
          setSummary({
            total_debits: 25500.00,
            total_credits: 82000.00,
            variance: -56500.00,
            is_balanced: false,
            period: 'Current Period',
            as_at_date: new Date().toISOString().split('T')[0]
          })
        }
      } catch (error) {
        console.error('Failed to fetch trial balance:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchTrialBalance()
  }, [selectedPeriod])

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        <PrinterIcon className="h-4 w-4" />
        Print
      </Button>
      <Button variant="outline" size="sm">
        <ArrowDownTrayIcon className="h-4 w-4" />
        Export
      </Button>
      <Button size="sm">
        <CalendarIcon className="h-4 w-4" />
        Change Period
      </Button>
    </div>
  )

  const columns = [
    {
      key: 'account_code',
      header: 'Account Code',
      className: 'w-32 font-mono',
      render: (value: any) => (
        <span className="font-mono text-sm font-medium">{value}</span>
      )
    },
    {
      key: 'account_name',
      header: 'Account Name',
      className: 'min-w-[250px]',
      render: (value: any, row: TrialBalanceItem) => (
        <div>
          <div className="font-medium text-gray-900">{value}</div>
          <div className="text-sm text-gray-500">{row.account_type}</div>
        </div>
      )
    },
    {
      key: 'debit_balance',
      header: 'Debit',
      className: 'w-32 text-right',
      render: (value: any) => (
        <div className="text-right font-mono">
          {value > 0 ? formatCurrency(value) : ''}
        </div>
      )
    },
    {
      key: 'credit_balance',
      header: 'Credit',
      className: 'w-32 text-right',
      render: (value: any) => (
        <div className="text-right font-mono">
          {value > 0 ? formatCurrency(value) : ''}
        </div>
      )
    }
  ]

  // Add totals row to data
  const dataWithTotals = [
    ...trialBalance,
    {
      account_code: '',
      account_name: 'TOTALS',
      debit_balance: summary?.total_debits || 0,
      credit_balance: summary?.total_credits || 0,
      account_type: ''
    }
  ]

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Trial Balance"
        description="View the trial balance for the selected period"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'General Ledger', href: '/gl' },
          { label: 'Trial Balance' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Card */}
        {summary && (
          <div className="mb-8 grid grid-cols-1 md:grid-cols-3 gap-6">
            <Card>
              <div className="p-6 text-center">
                <div className={`mx-auto h-12 w-12 rounded-full flex items-center justify-center ${
                  summary.is_balanced ? 'bg-green-100' : 'bg-red-100'
                }`}>
                  {summary.is_balanced ? (
                    <CheckCircleIcon className="h-6 w-6 text-green-600" />
                  ) : (
                    <ExclamationTriangleIcon className="h-6 w-6 text-red-600" />
                  )}
                </div>
                <h3 className="mt-4 text-lg font-medium text-gray-900">
                  {summary.is_balanced ? 'In Balance' : 'Out of Balance'}
                </h3>
                <p className="text-sm text-gray-500">Trial Balance Status</p>
              </div>
            </Card>

            <Card>
              <div className="p-6">
                <div className="flex justify-between items-center mb-4">
                  <h4 className="text-sm font-medium text-gray-500">Total Debits</h4>
                  <span className="text-lg font-semibold text-gray-900">
                    {formatCurrency(summary.total_debits)}
                  </span>
                </div>
                <div className="flex justify-between items-center">
                  <h4 className="text-sm font-medium text-gray-500">Total Credits</h4>
                  <span className="text-lg font-semibold text-gray-900">
                    {formatCurrency(summary.total_credits)}
                  </span>
                </div>
                {summary.variance !== 0 && (
                  <div className="mt-4 pt-4 border-t border-gray-200">
                    <div className="flex justify-between items-center">
                      <h4 className="text-sm font-medium text-red-600">Variance</h4>
                      <span className="text-lg font-semibold text-red-600">
                        {formatCurrency(Math.abs(summary.variance))}
                      </span>
                    </div>
                  </div>
                )}
              </div>
            </Card>

            <Card>
              <div className="p-6">
                <h4 className="text-sm font-medium text-gray-500 mb-2">Period Information</h4>
                <div className="space-y-2">
                  <div className="flex justify-between">
                    <span className="text-sm text-gray-600">Period:</span>
                    <span className="text-sm font-medium text-gray-900">{summary.period}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-sm text-gray-600">As at:</span>
                    <span className="text-sm font-medium text-gray-900">
                      {new Date(summary.as_at_date).toLocaleDateString()}
                    </span>
                  </div>
                </div>
              </div>
            </Card>
          </div>
        )}

        {/* Period Selection */}
        <div className="mb-6">
          <div className="flex items-center space-x-4">
            <label className="text-sm font-medium text-gray-700">Period:</label>
            <select
              value={selectedPeriod}
              onChange={(e) => setSelectedPeriod(e.target.value)}
              className="rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
            >
              <option value="current">Current Period</option>
              <option value="previous">Previous Period</option>
              <option value="ytd">Year to Date</option>
            </select>
          </div>
        </div>

        {/* Trial Balance Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <div className="flex items-center justify-between">
              <div>
                <h3 className="text-lg font-medium text-gray-900 flex items-center">
                  <ScaleIcon className="h-5 w-5 mr-2" />
                  Trial Balance
                </h3>
                <p className="text-sm text-gray-500">
                  {summary?.period} - As at {summary && new Date(summary.as_at_date).toLocaleDateString()}
                </p>
              </div>
            </div>
          </div>
          <Table
            data={dataWithTotals}
            columns={columns}
            loading={loading}
            emptyMessage="No trial balance data available"
            className="trial-balance-table"
          />
        </Card>
      </main>

      <style jsx>{`
        .trial-balance-table :global(tbody tr:last-child) {
          font-weight: bold;
          border-top: 2px solid #e5e7eb;
          background-color: #f9fafb;
        }
      `}</style>
    </div>
  )
}