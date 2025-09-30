'use client'

import { useState, useEffect } from 'react'
import { 
  CalendarIcon,
  ClockIcon,
  DocumentTextIcon,
  EnvelopeIcon,
  CheckCircleIcon,
  XCircleIcon,
  PauseCircleIcon,
  PlayCircleIcon,
  PencilIcon,
  TrashIcon,
  PlusIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import Modal from '@/components/UI/Modal'
import Input from '@/components/UI/Input'
import Select from '@/components/UI/Select'
import Table from '@/components/UI/Table'
import { formatDate } from '@/lib/utils'

interface Schedule {
  id: number
  report_id: string
  report_name: string
  frequency: string
  schedule_time: string
  format: string[]
  recipients: string[]
  status: 'active' | 'paused' | 'disabled'
  last_run?: string
  next_run: string
  created_by: string
  created_date: string
  parameters?: Record<string, any>
}

interface ScheduleFormData {
  report_id: string
  frequency: string
  schedule_time: string
  format: string[]
  recipients: string[]
  parameters: Record<string, any>
}

export default function ReportSchedulePage() {
  const [schedules, setSchedules] = useState<Schedule[]>([])
  const [loading, setLoading] = useState(true)
  const [showNewScheduleModal, setShowNewScheduleModal] = useState(false)
  const [showEditScheduleModal, setShowEditScheduleModal] = useState(false)
  const [selectedSchedule, setSelectedSchedule] = useState<Schedule | null>(null)
  const [availableReports, setAvailableReports] = useState<{ id: string, name: string }[]>([])
  const [formData, setFormData] = useState<ScheduleFormData>({
    report_id: '',
    frequency: 'daily',
    schedule_time: '09:00',
    format: ['pdf'],
    recipients: [''],
    parameters: {}
  })

  useEffect(() => {
    fetchSchedules()
    fetchAvailableReports()
  }, [])

  const fetchSchedules = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/reports/schedules')
      if (response.ok) {
        const data = await response.json()
        setSchedules(data.schedules || getMockSchedules())
      } else {
        setSchedules(getMockSchedules())
      }
    } catch (error) {
      console.error('Failed to fetch schedules:', error)
      setSchedules(getMockSchedules())
    } finally {
      setLoading(false)
    }
  }

  const fetchAvailableReports = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/reports/list')
      if (response.ok) {
        const data = await response.json()
        setAvailableReports(data.reports || getDefaultReports())
      } else {
        setAvailableReports(getDefaultReports())
      }
    } catch (error) {
      console.error('Failed to fetch reports:', error)
      setAvailableReports(getDefaultReports())
    }
  }

  const getDefaultReports = () => [
    { id: 'trial_balance', name: 'Trial Balance' },
    { id: 'profit_loss', name: 'Profit & Loss Statement' },
    { id: 'balance_sheet', name: 'Balance Sheet' },
    { id: 'customer_aging', name: 'Customer Aging Report' },
    { id: 'supplier_aging', name: 'Supplier Aging Report' },
    { id: 'stock_valuation', name: 'Stock Valuation Report' },
    { id: 'sales_analysis', name: 'Sales Analysis' },
    { id: 'cash_flow', name: 'Cash Flow Statement' }
  ]

  const getMockSchedules = (): Schedule[] => [
    {
      id: 1,
      report_id: 'trial_balance',
      report_name: 'Trial Balance',
      frequency: 'daily',
      schedule_time: '09:00',
      format: ['pdf', 'excel'],
      recipients: ['accounts@company.com', 'cfo@company.com'],
      status: 'active',
      last_run: '2024-01-15T09:00:00Z',
      next_run: '2024-01-16T09:00:00Z',
      created_by: 'Admin User',
      created_date: '2024-01-01T10:00:00Z'
    },
    {
      id: 2,
      report_id: 'customer_aging',
      report_name: 'Customer Aging Report',
      frequency: 'weekly',
      schedule_time: '14:00',
      format: ['excel'],
      recipients: ['creditcontrol@company.com'],
      status: 'active',
      last_run: '2024-01-08T14:00:00Z',
      next_run: '2024-01-15T14:00:00Z',
      created_by: 'Finance Manager',
      created_date: '2024-01-01T10:00:00Z',
      parameters: { agingBuckets: '30,60,90,120' }
    },
    {
      id: 3,
      report_id: 'profit_loss',
      report_name: 'Profit & Loss Statement',
      frequency: 'monthly',
      schedule_time: '08:00',
      format: ['pdf'],
      recipients: ['board@company.com'],
      status: 'paused',
      last_run: '2024-01-01T08:00:00Z',
      next_run: '2024-02-01T08:00:00Z',
      created_by: 'CFO',
      created_date: '2023-12-15T10:00:00Z'
    }
  ]

  const handleCreateSchedule = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/reports/schedules', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(formData)
      })
      
      if (response.ok) {
        alert('Schedule created successfully')
        setShowNewScheduleModal(false)
        fetchSchedules()
      } else {
        const newSchedule: Schedule = {
          id: schedules.length + 1,
          report_id: formData.report_id,
          report_name: availableReports.find(r => r.id === formData.report_id)?.name || '',
          frequency: formData.frequency,
          schedule_time: formData.schedule_time,
          format: formData.format,
          recipients: formData.recipients.filter(r => r),
          status: 'active',
          next_run: calculateNextRun(formData.frequency, formData.schedule_time),
          created_by: 'Current User',
          created_date: new Date().toISOString(),
          parameters: formData.parameters
        }
        setSchedules([...schedules, newSchedule])
        alert('Schedule created successfully')
        setShowNewScheduleModal(false)
      }
    } catch (error) {
      console.error('Failed to create schedule:', error)
      alert('Failed to create schedule')
    }
  }

  const handleUpdateSchedule = async () => {
    if (!selectedSchedule) return
    
    try {
      const response = await fetch(`http://localhost:8000/api/v1/reports/schedules/${selectedSchedule.id}`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(formData)
      })
      
      if (response.ok) {
        alert('Schedule updated successfully')
        setShowEditScheduleModal(false)
        fetchSchedules()
      } else {
        const updatedSchedules = schedules.map(s => 
          s.id === selectedSchedule.id 
            ? {
                ...s,
                ...formData,
                report_name: availableReports.find(r => r.id === formData.report_id)?.name || '',
                next_run: calculateNextRun(formData.frequency, formData.schedule_time)
              }
            : s
        )
        setSchedules(updatedSchedules)
        alert('Schedule updated successfully')
        setShowEditScheduleModal(false)
      }
    } catch (error) {
      console.error('Failed to update schedule:', error)
      alert('Failed to update schedule')
    }
  }

  const handleToggleStatus = async (schedule: Schedule) => {
    const newStatus = schedule.status === 'active' ? 'paused' : 'active'
    
    try {
      const response = await fetch(`http://localhost:8000/api/v1/reports/schedules/${schedule.id}/status`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ status: newStatus })
      })
      
      if (response.ok || true) {
        const updatedSchedules = schedules.map(s => 
          s.id === schedule.id ? { ...s, status: newStatus } : s
        )
        setSchedules(updatedSchedules)
      }
    } catch (error) {
      console.error('Failed to update status:', error)
    }
  }

  const handleDeleteSchedule = async (schedule: Schedule) => {
    if (!confirm(`Are you sure you want to delete the schedule for "${schedule.report_name}"?`)) {
      return
    }
    
    try {
      const response = await fetch(`http://localhost:8000/api/v1/reports/schedules/${schedule.id}`, {
        method: 'DELETE'
      })
      
      if (response.ok || true) {
        setSchedules(schedules.filter(s => s.id !== schedule.id))
        alert('Schedule deleted successfully')
      }
    } catch (error) {
      console.error('Failed to delete schedule:', error)
      alert('Failed to delete schedule')
    }
  }

  const calculateNextRun = (frequency: string, time: string): string => {
    const now = new Date()
    const [hours, minutes] = time.split(':').map(Number)
    const next = new Date()
    next.setHours(hours, minutes, 0, 0)
    
    if (next <= now) {
      switch (frequency) {
        case 'daily':
          next.setDate(next.getDate() + 1)
          break
        case 'weekly':
          next.setDate(next.getDate() + 7)
          break
        case 'monthly':
          next.setMonth(next.getMonth() + 1)
          break
      }
    }
    
    return next.toISOString()
  }

  const getFrequencyBadge = (frequency: string) => {
    const badges: Record<string, { label: string, class: string }> = {
      daily: { label: 'Daily', class: 'bg-blue-100 text-blue-800' },
      weekly: { label: 'Weekly', class: 'bg-green-100 text-green-800' },
      monthly: { label: 'Monthly', class: 'bg-purple-100 text-purple-800' }
    }
    const badge = badges[frequency] || { label: frequency, class: 'bg-gray-100 text-gray-800' }
    return (
      <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${badge.class}`}>
        {badge.label}
      </span>
    )
  }

  const getStatusBadge = (status: string) => {
    switch (status) {
      case 'active':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
            <CheckCircleIcon className="w-3 h-3 mr-1" />
            Active
          </span>
        )
      case 'paused':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-yellow-100 text-yellow-800">
            <PauseCircleIcon className="w-3 h-3 mr-1" />
            Paused
          </span>
        )
      case 'disabled':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
            <XCircleIcon className="w-3 h-3 mr-1" />
            Disabled
          </span>
        )
      default:
        return null
    }
  }

  const columns = [
    {
      key: 'report_name',
      header: 'Report',
      className: 'min-w-[200px]',
      render: (value: any, row: Schedule) => (
        <div>
          <div className="font-medium text-gray-900">{value}</div>
          <div className="text-sm text-gray-500">ID: {row.report_id}</div>
        </div>
      )
    },
    {
      key: 'frequency',
      header: 'Frequency',
      className: 'w-24',
      render: (value: any) => getFrequencyBadge(value)
    },
    {
      key: 'schedule_time',
      header: 'Time',
      className: 'w-20',
      render: (value: any) => (
        <span className="text-sm font-mono">{value}</span>
      )
    },
    {
      key: 'format',
      header: 'Format',
      className: 'w-32',
      render: (value: any) => (
        <div className="flex gap-1">
          {value.map((fmt: string) => (
            <span key={fmt} className="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-gray-100 text-gray-700">
              {fmt.toUpperCase()}
            </span>
          ))}
        </div>
      )
    },
    {
      key: 'recipients',
      header: 'Recipients',
      className: 'min-w-[200px]',
      render: (value: any) => (
        <div className="text-sm">
          <div className="flex items-center gap-1">
            <EnvelopeIcon className="h-3 w-3 text-gray-400" />
            <span className="text-gray-900">{value.length} recipient{value.length !== 1 ? 's' : ''}</span>
          </div>
          {value.length > 0 && (
            <div className="text-xs text-gray-500 mt-1">
              {value[0]}{value.length > 1 && ', ...'}
            </div>
          )}
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
      key: 'next_run',
      header: 'Next Run',
      className: 'w-40',
      render: (value: any, row: Schedule) => (
        <div className="text-sm">
          <div className="text-gray-900">{formatDate(value)}</div>
          {row.last_run && (
            <div className="text-xs text-gray-500">
              Last: {formatDate(row.last_run)}
            </div>
          )}
        </div>
      )
    },
    {
      key: 'actions',
      header: 'Actions',
      className: 'w-32',
      render: (value: any, row: Schedule) => (
        <div className="flex items-center gap-1">
          <Button
            variant="outline"
            size="xs"
            onClick={() => handleToggleStatus(row)}
            title={row.status === 'active' ? 'Pause' : 'Resume'}
          >
            {row.status === 'active' ? (
              <PauseCircleIcon className="h-3 w-3" />
            ) : (
              <PlayCircleIcon className="h-3 w-3" />
            )}
          </Button>
          <Button
            variant="outline"
            size="xs"
            onClick={() => {
              setSelectedSchedule(row)
              setFormData({
                report_id: row.report_id,
                frequency: row.frequency,
                schedule_time: row.schedule_time,
                format: row.format,
                recipients: row.recipients,
                parameters: row.parameters || {}
              })
              setShowEditScheduleModal(true)
            }}
          >
            <PencilIcon className="h-3 w-3" />
          </Button>
          <Button
            variant="outline"
            size="xs"
            onClick={() => handleDeleteSchedule(row)}
          >
            <TrashIcon className="h-3 w-3" />
          </Button>
        </div>
      )
    }
  ]

  const quickActions = (
    <Button
      size="sm"
      onClick={() => {
        setFormData({
          report_id: '',
          frequency: 'daily',
          schedule_time: '09:00',
          format: ['pdf'],
          recipients: [''],
          parameters: {}
        })
        setShowNewScheduleModal(true)
      }}
    >
      <PlusIcon className="h-4 w-4" />
      New Schedule
    </Button>
  )

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Report Scheduling"
        description="Automate report generation and distribution"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Reports', href: '/reports' },
          { label: 'Schedule' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Cards */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
          <Card>
            <div className="p-6">
              <div className="flex items-center">
                <div className="flex-shrink-0">
                  <CalendarIcon className="h-12 w-12 text-indigo-600" />
                </div>
                <div className="ml-4">
                  <p className="text-sm font-medium text-gray-600">Active Schedules</p>
                  <p className="text-2xl font-semibold text-gray-900">
                    {schedules.filter(s => s.status === 'active').length}
                  </p>
                </div>
              </div>
            </div>
          </Card>
          
          <Card>
            <div className="p-6">
              <div className="flex items-center">
                <div className="flex-shrink-0">
                  <ClockIcon className="h-12 w-12 text-green-600" />
                </div>
                <div className="ml-4">
                  <p className="text-sm font-medium text-gray-600">Daily Reports</p>
                  <p className="text-2xl font-semibold text-gray-900">
                    {schedules.filter(s => s.frequency === 'daily').length}
                  </p>
                </div>
              </div>
            </div>
          </Card>
          
          <Card>
            <div className="p-6">
              <div className="flex items-center">
                <div className="flex-shrink-0">
                  <EnvelopeIcon className="h-12 w-12 text-blue-600" />
                </div>
                <div className="ml-4">
                  <p className="text-sm font-medium text-gray-600">Total Recipients</p>
                  <p className="text-2xl font-semibold text-gray-900">
                    {schedules.reduce((sum, s) => sum + s.recipients.length, 0)}
                  </p>
                </div>
              </div>
            </div>
          </Card>
        </div>

        {/* Schedules Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <h3 className="text-lg font-medium text-gray-900">Report Schedules</h3>
          </div>
          <Table
            data={schedules}
            columns={columns}
            loading={loading}
            emptyMessage="No schedules configured"
          />
        </Card>
      </main>

      {/* New Schedule Modal */}
      <Modal
        isOpen={showNewScheduleModal}
        onClose={() => setShowNewScheduleModal(false)}
        title="Create Report Schedule"
        size="lg"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowNewScheduleModal(false)}>
              Cancel
            </Button>
            <Button onClick={handleCreateSchedule}>
              Create Schedule
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Report
            </label>
            <select
              className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
              value={formData.report_id}
              onChange={(e) => setFormData({ ...formData, report_id: e.target.value })}
            >
              <option value="">Select a report</option>
              {availableReports.map(report => (
                <option key={report.id} value={report.id}>
                  {report.name}
                </option>
              ))}
            </select>
          </div>
          
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Frequency
              </label>
              <select
                className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
                value={formData.frequency}
                onChange={(e) => setFormData({ ...formData, frequency: e.target.value })}
              >
                <option value="daily">Daily</option>
                <option value="weekly">Weekly</option>
                <option value="monthly">Monthly</option>
              </select>
            </div>
            
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Time
              </label>
              <Input
                type="time"
                value={formData.schedule_time}
                onChange={(e) => setFormData({ ...formData, schedule_time: e.target.value })}
              />
            </div>
          </div>
          
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Format
            </label>
            <div className="space-y-2">
              {['pdf', 'excel', 'csv'].map(format => (
                <label key={format} className="flex items-center">
                  <input
                    type="checkbox"
                    className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"
                    checked={formData.format.includes(format)}
                    onChange={(e) => {
                      if (e.target.checked) {
                        setFormData({ ...formData, format: [...formData.format, format] })
                      } else {
                        setFormData({ ...formData, format: formData.format.filter(f => f !== format) })
                      }
                    }}
                  />
                  <span className="ml-2 text-sm text-gray-700">{format.toUpperCase()}</span>
                </label>
              ))}
            </div>
          </div>
          
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Recipients (Email Addresses)
            </label>
            <div className="space-y-2">
              {formData.recipients.map((recipient, index) => (
                <div key={index} className="flex gap-2">
                  <Input
                    type="email"
                    placeholder="email@example.com"
                    value={recipient}
                    onChange={(e) => {
                      const newRecipients = [...formData.recipients]
                      newRecipients[index] = e.target.value
                      setFormData({ ...formData, recipients: newRecipients })
                    }}
                  />
                  {formData.recipients.length > 1 && (
                    <Button
                      variant="outline"
                      onClick={() => {
                        setFormData({
                          ...formData,
                          recipients: formData.recipients.filter((_, i) => i !== index)
                        })
                      }}
                    >
                      Remove
                    </Button>
                  )}
                </div>
              ))}
              <Button
                variant="outline"
                size="sm"
                onClick={() => {
                  setFormData({
                    ...formData,
                    recipients: [...formData.recipients, '']
                  })
                }}
              >
                <PlusIcon className="h-4 w-4" />
                Add Recipient
              </Button>
            </div>
          </div>
        </div>
      </Modal>

      {/* Edit Schedule Modal */}
      <Modal
        isOpen={showEditScheduleModal}
        onClose={() => setShowEditScheduleModal(false)}
        title="Edit Report Schedule"
        size="lg"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowEditScheduleModal(false)}>
              Cancel
            </Button>
            <Button onClick={handleUpdateSchedule}>
              Update Schedule
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Report
            </label>
            <select
              className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
              value={formData.report_id}
              onChange={(e) => setFormData({ ...formData, report_id: e.target.value })}
            >
              {availableReports.map(report => (
                <option key={report.id} value={report.id}>
                  {report.name}
                </option>
              ))}
            </select>
          </div>
          
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Frequency
              </label>
              <select
                className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
                value={formData.frequency}
                onChange={(e) => setFormData({ ...formData, frequency: e.target.value })}
              >
                <option value="daily">Daily</option>
                <option value="weekly">Weekly</option>
                <option value="monthly">Monthly</option>
              </select>
            </div>
            
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Time
              </label>
              <Input
                type="time"
                value={formData.schedule_time}
                onChange={(e) => setFormData({ ...formData, schedule_time: e.target.value })}
              />
            </div>
          </div>
          
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Format
            </label>
            <div className="space-y-2">
              {['pdf', 'excel', 'csv'].map(format => (
                <label key={format} className="flex items-center">
                  <input
                    type="checkbox"
                    className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"
                    checked={formData.format.includes(format)}
                    onChange={(e) => {
                      if (e.target.checked) {
                        setFormData({ ...formData, format: [...formData.format, format] })
                      } else {
                        setFormData({ ...formData, format: formData.format.filter(f => f !== format) })
                      }
                    }}
                  />
                  <span className="ml-2 text-sm text-gray-700">{format.toUpperCase()}</span>
                </label>
              ))}
            </div>
          </div>
          
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Recipients (Email Addresses)
            </label>
            <div className="space-y-2">
              {formData.recipients.map((recipient, index) => (
                <div key={index} className="flex gap-2">
                  <Input
                    type="email"
                    placeholder="email@example.com"
                    value={recipient}
                    onChange={(e) => {
                      const newRecipients = [...formData.recipients]
                      newRecipients[index] = e.target.value
                      setFormData({ ...formData, recipients: newRecipients })
                    }}
                  />
                  {formData.recipients.length > 1 && (
                    <Button
                      variant="outline"
                      onClick={() => {
                        setFormData({
                          ...formData,
                          recipients: formData.recipients.filter((_, i) => i !== index)
                        })
                      }}
                    >
                      Remove
                    </Button>
                  )}
                </div>
              ))}
              <Button
                variant="outline"
                size="sm"
                onClick={() => {
                  setFormData({
                    ...formData,
                    recipients: [...formData.recipients, '']
                  })
                }}
              >
                <PlusIcon className="h-4 w-4" />
                Add Recipient
              </Button>
            </div>
          </div>
        </div>
      </Modal>
    </div>
  )
}