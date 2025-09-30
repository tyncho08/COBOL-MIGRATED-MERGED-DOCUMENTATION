'use client'

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
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
import Modal from '@/components/UI/Modal'
import Input from '@/components/UI/Input'
import Select from '@/components/UI/Select'

interface GLPeriod {
  period_id: number
  period_name: string
  start_date: string
  end_date: string
  is_open: boolean
  is_adjustment_period: boolean
  fiscal_year: number
  period_number: number
  closed_by?: string
  closed_date?: string
}

export default function GLPeriodsPage() {
  const router = useRouter()
  const [periods, setPeriods] = useState<GLPeriod[]>([])
  const [loading, setLoading] = useState(true)
  const [currentPeriod, setCurrentPeriod] = useState<GLPeriod | null>(null)
  const [showNewPeriodModal, setShowNewPeriodModal] = useState(false)
  const [showCloseModal, setShowCloseModal] = useState(false)
  const [selectedPeriod, setSelectedPeriod] = useState<GLPeriod | null>(null)
  const [periodForm, setPeriodForm] = useState({
    period_name: '',
    start_date: '',
    end_date: '',
    fiscal_year: new Date().getFullYear(),
    period_number: 1,
    is_adjustment_period: false
  })

  useEffect(() => {
    const fetchPeriods = async () => {
      try {
        const response = await fetch('http://localhost:8000/api/v1/gl/periods')
        if (response.ok) {
          const data = await response.json()
          setPeriods(data || [])
          setCurrentPeriod(data.find((p: any) => p.is_open) || null)
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
      <Button variant="outline" size="sm" onClick={() => setShowNewPeriodModal(true)}>
        <CalendarIcon className="h-4 w-4" />
        New Period
      </Button>
      <Button 
        size="sm" 
        disabled={!currentPeriod || !currentPeriod.is_open}
        onClick={() => {
          if (currentPeriod) {
            setSelectedPeriod(currentPeriod)
            setShowCloseModal(true)
          }
        }}
      >
        <LockClosedIcon className="h-4 w-4" />
        Close Current Period
      </Button>
    </div>
  )

  const getStatusBadge = (isOpen: boolean) => {
    if (isOpen) {
      return (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
          <LockOpenIcon className="w-3 h-3 mr-1" />
          Open
        </span>
      )
    } else {
      return (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
          <LockClosedIcon className="w-3 h-3 mr-1" />
          Closed
        </span>
      )
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
      key: 'is_open',
      header: 'Status',
      className: 'w-24',
      render: (value: any) => getStatusBadge(value)
    },
    {
      key: 'fiscal_year',
      header: 'Fiscal Year',
      className: 'w-24 text-center',
      render: (value: any) => (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
          {value}
        </span>
      )
    },
    {
      key: 'period_number',
      header: 'Period #',
      className: 'w-24 text-center',
      render: (value: any) => (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
          {value}
        </span>
      )
    },
    {
      key: 'actions',
      header: 'Actions',
      className: 'w-32',
      render: (value: any, row: GLPeriod) => (
        <div className="flex space-x-1">
          {row.is_open && (
            <Button 
              variant="outline" 
              size="xs"
              onClick={() => {
                setSelectedPeriod(row)
                setShowCloseModal(true)
              }}
            >
              Close
            </Button>
          )}
          <Button 
            variant="outline" 
            size="xs"
            onClick={() => router.push(`/gl/periods/${row.period_id}`)}
          >
            View
          </Button>
        </div>
      )
    }
  ]

  const openPeriods = periods.filter(p => p.is_open).length

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
              value: currentPeriod?.is_open ? 'Open for posting' : 'Closed', 
              type: currentPeriod?.is_open ? 'increase' : 'neutral' 
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
            title="Total Periods"
            value={periods.length.toString()}
            icon={<ClockIcon className="h-6 w-6" />}
            change={{ 
              value: 'All fiscal years', 
              type: 'neutral' 
            }}
          />
          <StatsCard
            title="Period Status"
            value="Out of Balance"
            icon={<ExclamationTriangleIcon className="h-6 w-6" />}
            change={{ 
              value: '£0.00', 
              type: 'decrease' 
            }}
          />
        </div>


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
              <Button 
                variant="outline" 
                size="sm"
                onClick={() => router.push('/settings?tab=periods')}
              >
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

      {/* New Period Modal */}
      <Modal
        isOpen={showNewPeriodModal}
        onClose={() => {
          setShowNewPeriodModal(false)
          setPeriodForm({
            period_name: '',
            start_date: '',
            end_date: '',
            fiscal_year: new Date().getFullYear(),
            period_number: 1,
            is_adjustment_period: false
          })
        }}
        title="Create New Period"
        size="md"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowNewPeriodModal(false)}>
              Cancel
            </Button>
            <Button 
              className="ml-2"
              onClick={async () => {
                try {
                  const response = await fetch('http://localhost:8000/api/v1/gl/periods', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify(periodForm)
                  })
                  
                  const data = await response.json()
                  if (data.success) {
                    alert('Period created successfully!')
                    setShowNewPeriodModal(false)
                    window.location.reload()
                  } else {
                    alert(data.message || 'Failed to create period')
                  }
                } catch (error) {
                  console.error('Error creating period:', error)
                  alert('Failed to create period')
                }
              }}
            >
              Create Period
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <Input
            label="Period Name"
            type="text"
            value={periodForm.period_name}
            onChange={(e) => setPeriodForm({...periodForm, period_name: e.target.value})}
            placeholder="January 2024"
            required
          />
          <div className="grid grid-cols-2 gap-4">
            <Input
              label="Start Date"
              type="date"
              value={periodForm.start_date}
              onChange={(e) => setPeriodForm({...periodForm, start_date: e.target.value})}
              required
            />
            <Input
              label="End Date"
              type="date"
              value={periodForm.end_date}
              onChange={(e) => setPeriodForm({...periodForm, end_date: e.target.value})}
              required
            />
          </div>
          <div className="grid grid-cols-2 gap-4">
            <Input
              label="Fiscal Year"
              type="number"
              value={periodForm.fiscal_year}
              onChange={(e) => setPeriodForm({...periodForm, fiscal_year: parseInt(e.target.value)})}
              min="2000"
              max="2100"
              required
            />
            <Input
              label="Period Number"
              type="number"
              value={periodForm.period_number}
              onChange={(e) => setPeriodForm({...periodForm, period_number: parseInt(e.target.value)})}
              min="1"
              max="13"
              required
            />
          </div>
          <div className="flex items-center">
            <input
              type="checkbox"
              id="is_adjustment"
              checked={periodForm.is_adjustment_period}
              onChange={(e) => setPeriodForm({...periodForm, is_adjustment_period: e.target.checked})}
              className="h-4 w-4 text-indigo-600 focus:ring-indigo-500 border-gray-300 rounded"
            />
            <label htmlFor="is_adjustment" className="ml-2 text-sm text-gray-700">
              Adjustment Period (Year-End)
            </label>
          </div>
        </div>
      </Modal>

      {/* Close Period Modal */}
      <Modal
        isOpen={showCloseModal}
        onClose={() => {
          setShowCloseModal(false)
          setSelectedPeriod(null)
        }}
        title="Close Period"
        size="md"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowCloseModal(false)}>
              Cancel
            </Button>
            <Button 
              className="ml-2"
              variant="danger"
              onClick={async () => {
                if (!selectedPeriod) return
                
                if (!confirm(`Are you sure you want to close period "${selectedPeriod.period_name}"? This action cannot be undone.`)) {
                  return
                }
                
                try {
                  const response = await fetch(`http://localhost:8000/api/v1/gl/periods/${selectedPeriod.period_id}/close`, {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' }
                  })
                  
                  const data = await response.json()
                  if (data.success) {
                    alert(`Period "${selectedPeriod.period_name}" has been closed successfully!`)
                    setShowCloseModal(false)
                    window.location.reload()
                  } else {
                    alert(data.message || 'Failed to close period')
                  }
                } catch (error) {
                  console.error('Error closing period:', error)
                  alert('Failed to close period')
                }
              }}
            >
              Close Period
            </Button>
          </>
        }
      >
        {selectedPeriod && (
          <div className="space-y-4">
            <div className="rounded-lg bg-yellow-50 p-4">
              <div className="flex">
                <div className="flex-shrink-0">
                  <ExclamationTriangleIcon className="h-5 w-5 text-yellow-400" />
                </div>
                <div className="ml-3">
                  <h3 className="text-sm font-medium text-yellow-800">
                    Period Closure Warning
                  </h3>
                  <div className="mt-2 text-sm text-yellow-700">
                    <p>Closing a period will:</p>
                    <ul className="list-disc pl-5 space-y-1 mt-1">
                      <li>Prevent any new journal entries being posted to this period</li>
                      <li>Lock all transactions in this period</li>
                      <li>Calculate and post year-end adjustments (if applicable)</li>
                      <li>This action cannot be undone</li>
                    </ul>
                  </div>
                </div>
              </div>
            </div>
            
            <div>
              <h4 className="font-medium text-gray-900 mb-2">Period Details</h4>
              <dl className="space-y-2">
                <div className="flex justify-between">
                  <dt className="text-sm text-gray-500">Period:</dt>
                  <dd className="text-sm font-medium text-gray-900">{selectedPeriod.period_name}</dd>
                </div>
                <div className="flex justify-between">
                  <dt className="text-sm text-gray-500">Date Range:</dt>
                  <dd className="text-sm font-medium text-gray-900">
                    {new Date(selectedPeriod.start_date).toLocaleDateString()} - {new Date(selectedPeriod.end_date).toLocaleDateString()}
                  </dd>
                </div>
                <div className="flex justify-between">
                  <dt className="text-sm text-gray-500">Fiscal Year:</dt>
                  <dd className="text-sm font-medium text-gray-900">{selectedPeriod.fiscal_year}</dd>
                </div>
                <div className="flex justify-between">
                  <dt className="text-sm text-gray-500">Period Number:</dt>
                  <dd className="text-sm font-medium text-gray-900">{selectedPeriod.period_number}</dd>
                </div>
              </dl>
            </div>
          </div>
        )}
      </Modal>
    </div>
  )
}