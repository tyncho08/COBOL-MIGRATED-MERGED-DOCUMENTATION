'use client'

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
import {
  DocumentTextIcon,
  CheckCircleIcon,
  XCircleIcon,
  ClockIcon,
  PencilIcon,
  EyeIcon,
  TrashIcon,
  PlusIcon,
  FunnelIcon,
  ArrowDownTrayIcon,
  PrinterIcon,
  CalendarIcon,
  ScaleIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import PageHeader from '@/components/Layout/PageHeader'
import Button from '@/components/UI/Button'
import Input from '@/components/UI/Input'
import Select from '@/components/UI/Select'
import Table from '@/components/UI/Table'
import Modal from '@/components/UI/Modal'

interface JournalEntry {
  id: number
  journal_number: string
  description: string
  total_amount: number
  entry_date: string
  status: string
  created_by: string
}

interface JournalLine {
  id: string
  account_code: string
  account_name: string
  description: string
  debit_amount: number
  credit_amount: number
  cost_center?: string
  project_code?: string
}

export default function GLJournalsPage() {
  const router = useRouter()
  const [journals, setJournals] = useState<JournalEntry[]>([])
  const [loading, setLoading] = useState(true)
  const [selectedJournal, setSelectedJournal] = useState<JournalEntry | null>(null)
  const [showDetailModal, setShowDetailModal] = useState(false)
  const [filters, setFilters] = useState({
    dateFrom: '',
    dateTo: '',
    status: '',
    createdBy: '',
    search: ''
  })
  const [showFilters, setShowFilters] = useState(false)

  useEffect(() => {
    fetchJournals()
  }, [filters])

  const fetchJournals = async () => {
    setLoading(true)
    try {
      const queryParams = new URLSearchParams()
      Object.entries(filters).forEach(([key, value]) => {
        if (value) queryParams.append(key, value)
      })
      
      const response = await fetch(`http://localhost:8000/api/v1/gl/recent-journals`)
      if (response.ok) {
        const data = await response.json()
        setJournals(data || [])
      } else {
        console.error('Failed to fetch journals:', response.status)
        setJournals([])
      }
    } catch (error) {
      console.error('Failed to fetch journals:', error)
      setJournals([])
    } finally {
      setLoading(false)
    }
  }


  const getStatusColor = (status: string) => {
    switch (status) {
      case 'posted':
        return 'bg-green-100 text-green-700'
      case 'draft':
        return 'bg-yellow-100 text-yellow-700'
      case 'void':
        return 'bg-red-100 text-red-700'
      default:
        return 'bg-gray-100 text-gray-700'
    }
  }

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'posted':
        return <CheckCircleIcon className="h-4 w-4" />
      case 'draft':
        return <ClockIcon className="h-4 w-4" />
      case 'void':
        return <XCircleIcon className="h-4 w-4" />
      default:
        return <DocumentTextIcon className="h-4 w-4" />
    }
  }

  const formatCurrency = (value: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD'
    }).format(value)
  }

  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleDateString('en-GB', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric'
    })
  }

  const columns = [
    {
      key: 'journal_number',
      header: 'Journal Number',
      render: (_: any, journal: JournalEntry) => (
        <button
          onClick={() => {
            setSelectedJournal(journal)
            setShowDetailModal(true)
          }}
          className="text-indigo-600 hover:text-indigo-900 font-medium"
        >
          {journal.journal_number}
        </button>
      )
    },
    {
      key: 'entry_date',
      header: 'Date',
      render: (journal: JournalEntry) => formatDate(journal.entry_date)
    },
    {
      key: 'description',
      header: 'Description',
      render: (_: any, journal: JournalEntry) => (
        <div>
          <p className="font-medium text-gray-900">{journal.description}</p>
        </div>
      )
    },
    {
      key: 'status',
      header: 'Status',
      render: (_: any, journal: JournalEntry) => (
        <div className="flex items-center gap-2">
          <div className={`p-1 rounded ${getStatusColor(journal.status)}`}>
            {getStatusIcon(journal.status)}
          </div>
          <span className={`text-xs font-medium px-2 py-1 rounded-full ${getStatusColor(journal.status)}`}>
            {journal.status.toUpperCase()}
          </span>
        </div>
      )
    },
    {
      key: 'total_amount',
      header: 'Amount',
      render: (_: any, journal: JournalEntry) => (
        <div className="text-right">
          <p className="font-medium text-gray-900">{formatCurrency(journal.total_amount)}</p>
        </div>
      )
    },
    {
      key: 'created_by',
      header: 'Created By',
      render: (_: any, journal: JournalEntry) => (
        <div className="text-sm">
          <p className="font-medium text-gray-900">{journal.created_by}</p>
        </div>
      )
    },
    {
      key: 'actions',
      header: 'Actions',
      render: (_: any, journal: JournalEntry) => (
        <div className="flex items-center gap-2">
          <button
            onClick={() => {
              setSelectedJournal(journal)
              setShowDetailModal(true)
            }}
            className="p-1 text-gray-400 hover:text-gray-600"
          >
            <EyeIcon className="h-4 w-4" />
          </button>
        </div>
      )
    }
  ]


  const handleExport = (format: 'csv' | 'excel' | 'pdf') => {
    console.log(`Exporting journals as ${format}`)
    alert(`Export to ${format.toUpperCase()} functionality would be implemented here`)
  }

  const clearFilters = () => {
    setFilters({
      dateFrom: '',
      dateTo: '',
      status: '',
      createdBy: '',
      search: ''
    })
  }


  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Journal Entries"
        description="View and manage general ledger journal entries"
        actions={
          <div className="flex items-center gap-3">
            <Button
              onClick={() => router.push('/gl/journals/new')}
              icon={<PlusIcon className="h-4 w-4" />}
            >
              New Journal
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => setShowFilters(!showFilters)}
              icon={<FunnelIcon className="h-4 w-4" />}
            >
              Filters
              {Object.values(filters).some(v => v) && (
                <span className="ml-2 h-2 w-2 bg-indigo-600 rounded-full"></span>
              )}
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => handleExport('excel')}
              icon={<ArrowDownTrayIcon className="h-4 w-4" />}
            >
              Export
            </Button>
          </div>
        }
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'General Ledger', href: '/gl' },
          { label: 'Journal Entries' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Filters */}
        {showFilters && (
          <Card className="mb-6">
            <div className="p-6">
              <div className="grid grid-cols-1 md:grid-cols-5 gap-4">
                <Input
                  label="Date From"
                  type="date"
                  value={filters.dateFrom}
                  onChange={(e) => setFilters({ ...filters, dateFrom: e.target.value })}
                />
                <Input
                  label="Date To"
                  type="date"
                  value={filters.dateTo}
                  onChange={(e) => setFilters({ ...filters, dateTo: e.target.value })}
                />
                <Select
                  label="Status"
                  value={filters.status}
                  onChange={(e) => setFilters({ ...filters, status: e.target.value })}
                >
                  <option value="">All Statuses</option>
                  <option value="draft">Draft</option>
                  <option value="posted">Posted</option>
                  <option value="void">Void</option>
                </Select>
                <Input
                  label="Created By"
                  type="text"
                  value={filters.createdBy}
                  onChange={(e) => setFilters({ ...filters, createdBy: e.target.value })}
                  placeholder="Search user..."
                />
                <Input
                  label="Search"
                  type="text"
                  value={filters.search}
                  onChange={(e) => setFilters({ ...filters, search: e.target.value })}
                  placeholder="Search description..."
                />
              </div>
              <div className="mt-4 flex justify-end">
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={clearFilters}
                >
                  Clear Filters
                </Button>
              </div>
            </div>
          </Card>
        )}

        {/* Journal Summary */}
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
          <Card className="p-4">
            <div className="flex items-center">
              <DocumentTextIcon className="h-8 w-8 text-gray-500 mr-3" />
              <div>
                <p className="text-sm text-gray-500">Total Journals</p>
                <p className="text-xl font-bold text-gray-900">{journals.length}</p>
              </div>
            </div>
          </Card>
          <Card className="p-4">
            <div className="flex items-center">
              <CheckCircleIcon className="h-8 w-8 text-green-500 mr-3" />
              <div>
                <p className="text-sm text-gray-500">Posted</p>
                <p className="text-xl font-bold text-gray-900">
                  {journals.filter(j => j.status === 'posted').length}
                </p>
              </div>
            </div>
          </Card>
          <Card className="p-4">
            <div className="flex items-center">
              <ClockIcon className="h-8 w-8 text-yellow-500 mr-3" />
              <div>
                <p className="text-sm text-gray-500">Draft</p>
                <p className="text-xl font-bold text-gray-900">
                  {journals.filter(j => j.status === 'draft').length}
                </p>
              </div>
            </div>
          </Card>
          <Card className="p-4">
            <div className="flex items-center">
              <ScaleIcon className="h-8 w-8 text-indigo-500 mr-3" />
              <div>
                <p className="text-sm text-gray-500">Total Value</p>
                <p className="text-xl font-bold text-gray-900">
                  {formatCurrency(journals.reduce((sum, j) => sum + j.total_amount, 0))}
                </p>
              </div>
            </div>
          </Card>
        </div>

        {/* Journals Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <h3 className="text-lg font-medium text-gray-900">Journal Entries</h3>
          </div>
          <Table
            data={journals}
            columns={columns}
            loading={loading}
            emptyMessage="No journal entries found"
          />
        </Card>
      </main>

      {/* Journal Detail Modal */}
      <Modal
        isOpen={showDetailModal}
        onClose={() => setShowDetailModal(false)}
        title="Journal Entry Details"
        size="xl"
      >
        {selectedJournal && (
          <div className="space-y-6">
            {/* Journal Header */}
            <div className="bg-gray-50 p-4 rounded-lg">
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <p className="text-sm text-gray-500">Journal Number</p>
                  <p className="font-medium">{selectedJournal.journal_number}</p>
                </div>
                <div>
                  <p className="text-sm text-gray-500">Status</p>
                  <div className="flex items-center gap-2">
                    <span className={`text-xs font-medium px-2 py-1 rounded-full ${getStatusColor(selectedJournal.status)}`}>
                      {selectedJournal.status.toUpperCase()}
                    </span>
                  </div>
                </div>
                <div>
                  <p className="text-sm text-gray-500">Entry Date</p>
                  <p className="font-medium">{formatDate(selectedJournal.entry_date)}</p>
                </div>
                <div>
                  <p className="text-sm text-gray-500">Created By</p>
                  <p className="font-medium">{selectedJournal.created_by}</p>
                </div>
                <div className="col-span-2">
                  <p className="text-sm text-gray-500">Description</p>
                  <p className="font-medium">{selectedJournal.description}</p>
                </div>
              </div>
            </div>

            {/* Journal Summary */}
            <div>
              <h4 className="text-sm font-medium text-gray-900 mb-3">Journal Summary</h4>
              <div className="bg-gray-50 p-4 rounded-lg">
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <p className="text-sm text-gray-500">Total Amount</p>
                    <p className="text-lg font-medium text-gray-900">{formatCurrency(selectedJournal.total_amount)}</p>
                  </div>
                  <div>
                    <p className="text-sm text-gray-500">Status</p>
                    <span className={`text-xs font-medium px-2 py-1 rounded-full ${getStatusColor(selectedJournal.status)}`}>
                      {selectedJournal.status.toUpperCase()}
                    </span>
                  </div>
                </div>
              </div>
            </div>

            {/* Actions */}
            <div className="flex justify-end gap-3">
              <Button
                variant="ghost"
                onClick={() => setShowDetailModal(false)}
              >
                Close
              </Button>
            </div>
          </div>
        )}
      </Modal>
    </div>
  )
}