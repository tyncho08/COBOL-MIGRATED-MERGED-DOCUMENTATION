'use client'

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
import { 
  DocumentTextIcon,
  PlusIcon,
  MagnifyingGlassIcon,
  PrinterIcon,
  EnvelopeIcon,
  DocumentDuplicateIcon,
  TrashIcon,
  CheckCircleIcon,
  ClockIcon,
  XCircleIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import Input from '@/components/UI/Input'
import Table from '@/components/UI/Table'
import PageHeader from '@/components/Layout/PageHeader'
import Modal from '@/components/UI/Modal'
import Select from '@/components/UI/Select'
import { formatCurrency, formatDate } from '@/lib/utils'

interface Invoice {
  invoice_key: number
  invoice_number: string
  invoice_customer: string
  customer_name: string
  invoice_date: string
  invoice_due_date: string
  invoice_reference: string
  invoice_total_amount: number
  invoice_paid_amount: number
  invoice_balance: number
  invoice_status: 'D' | 'O' | 'P' | 'C' // Draft, Open, Paid, Cancelled
  invoice_lines: number
}

interface InvoiceFormData {
  customer_code: string
  invoice_date: string
  due_date: string
  reference: string
  lines: {
    description: string
    quantity: number
    unit_price: number
    amount: number
  }[]
}

export default function InvoicesPage() {
  const router = useRouter()
  const [invoices, setInvoices] = useState<Invoice[]>([])
  const [loading, setLoading] = useState(true)
  const [searchTerm, setSearchTerm] = useState('')
  const [showNewInvoiceModal, setShowNewInvoiceModal] = useState(false)
  const [selectedInvoices, setSelectedInvoices] = useState<Set<number>>(new Set())

  useEffect(() => {
    fetchInvoices()
  }, [])

  const fetchInvoices = async () => {
    try {
      setLoading(true)
      
      // Fetch real invoices from COBOL endpoint
      const response = await fetch('http://localhost:8000/api/v1/sales/cobol/invoices')
      if (response.ok) {
        const data = await response.json()
        setInvoices(data.invoices || [])
      } else {
        console.error('Failed to fetch invoices:', response.status)
      }
    } catch (error) {
      console.error('Failed to fetch invoices:', error)
    } finally {
      setLoading(false)
    }
  }

  // Filter invoices based on search
  const filteredInvoices = invoices.filter(invoice =>
    (invoice.invoice_number || '').toString().toLowerCase().includes(searchTerm.toLowerCase()) ||
    (invoice.customer_name || '').toString().toLowerCase().includes(searchTerm.toLowerCase()) ||
    (invoice.invoice_reference || '').toString().toLowerCase().includes(searchTerm.toLowerCase())
  )

  const handlePrintInvoices = () => {
    const selected = Array.from(selectedInvoices)
    if (selected.length === 0) {
      alert('Please select invoices to print')
      return
    }
    alert(`Printing ${selected.length} invoice(s)...`)
  }

  const handleEmailInvoices = () => {
    const selected = Array.from(selectedInvoices)
    if (selected.length === 0) {
      alert('Please select invoices to email')
      return
    }
    alert(`Emailing ${selected.length} invoice(s)...`)
  }

  const handleDuplicateInvoice = (invoice: Invoice) => {
    alert(`Creating copy of invoice ${invoice.invoice_number}...`)
  }

  const handleViewInvoice = (invoice: Invoice) => {
    alert(`Opening invoice ${invoice.invoice_number}...`)
  }

  const getStatusBadge = (status: string) => {
    switch (status) {
      case 'D':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
            <ClockIcon className="w-3 h-3 mr-1" />
            Draft
          </span>
        )
      case 'O':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-yellow-100 text-yellow-800">
            <ClockIcon className="w-3 h-3 mr-1" />
            Open
          </span>
        )
      case 'P':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
            <CheckCircleIcon className="w-3 h-3 mr-1" />
            Paid
          </span>
        )
      case 'C':
        return (
          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
            <XCircleIcon className="w-3 h-3 mr-1" />
            Cancelled
          </span>
        )
      default:
        return null
    }
  }

  const columns = [
    {
      key: 'invoice_number',
      header: 'Invoice #',
      render: (value: any, row: Invoice) => (
        <button
          onClick={() => handleViewInvoice(row)}
          className="font-medium text-blue-600 hover:text-blue-800"
        >
          {row.invoice_number}
        </button>
      )
    },
    {
      key: 'customer_name',
      header: 'Customer',
      render: (value: any, row: Invoice) => (
        <div>
          <div className="font-medium text-gray-900">{row.customer_name}</div>
          <div className="text-sm text-gray-500">{row.invoice_reference}</div>
        </div>
      )
    },
    {
      key: 'invoice_date',
      header: 'Date',
      render: (value: any, row: Invoice) => formatDate(row.invoice_date)
    },
    {
      key: 'invoice_due_date',
      header: 'Due Date',
      render: (value: any, row: Invoice) => (
        <div>
          <div>{formatDate(row.invoice_due_date)}</div>
          {new Date(row.invoice_due_date) < new Date() && row.invoice_balance > 0 && (
            <div className="text-sm text-red-600">Overdue</div>
          )}
        </div>
      )
    },
    {
      key: 'invoice_total_amount',
      header: 'Amount',
      render: (value: any, row: Invoice) => formatCurrency(row.invoice_total_amount)
    },
    {
      key: 'invoice_balance',
      header: 'Balance',
      render: (value: any, row: Invoice) => (
        <div className={`font-medium ${row.invoice_balance > 0 ? 'text-red-600' : 'text-green-600'}`}>
          {formatCurrency(row.invoice_balance)}
        </div>
      )
    },
    {
      key: 'invoice_status',
      header: 'Status',
      render: (value: any, row: Invoice) => getStatusBadge(row.invoice_status)
    },
    {
      key: 'actions',
      header: 'Actions',
      render: (value: any, row: Invoice) => (
        <div className="flex space-x-2">
          <button
            onClick={() => handleDuplicateInvoice(row)}
            className="text-gray-500 hover:text-gray-700"
          >
            <DocumentDuplicateIcon className="h-4 w-4" />
          </button>
          <button
            onClick={() => alert(`Delete invoice ${row.invoice_number}?`)}
            className="text-red-500 hover:text-red-700"
          >
            <TrashIcon className="h-4 w-4" />
          </button>
        </div>
      )
    }
  ]

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm" onClick={handlePrintInvoices}>
        <PrinterIcon className="h-4 w-4" />
        Print Selected
      </Button>
      <Button variant="outline" size="sm" onClick={handleEmailInvoices}>
        <EnvelopeIcon className="h-4 w-4" />
        Email Selected
      </Button>
      <Button size="sm" onClick={() => setShowNewInvoiceModal(true)}>
        <PlusIcon className="h-4 w-4" />
        New Invoice
      </Button>
    </div>
  )

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Sales Invoices"
        description="Manage customer invoices and billing"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Sales', href: '/sales' },
          { label: 'Invoices' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Search and Filters */}
        <Card className="mb-6">
          <div className="p-6">
            <div className="flex items-center space-x-4">
              <div className="flex-1">
                <Input
                  type="text"
                  placeholder="Search invoices..."
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                  leftIcon={<MagnifyingGlassIcon className="h-5 w-5" />}
                />
              </div>
              <Select
                value="all"
                onChange={() => {}}
                options={[
                  { value: 'all', label: 'All Invoices' },
                  { value: 'open', label: 'Open' },
                  { value: 'paid', label: 'Paid' },
                  { value: 'overdue', label: 'Overdue' },
                  { value: 'draft', label: 'Draft' }
                ]}
              />
              <Select
                value="this_month"
                onChange={() => {}}
                options={[
                  { value: 'today', label: 'Today' },
                  { value: 'this_week', label: 'This Week' },
                  { value: 'this_month', label: 'This Month' },
                  { value: 'last_month', label: 'Last Month' },
                  { value: 'custom', label: 'Custom Range' }
                ]}
              />
            </div>
          </div>
        </Card>

        {/* Invoices Table */}
        <Card>
          <Table
            data={filteredInvoices}
            columns={columns}
            loading={loading}
            emptyMessage="No invoices found"
            selection={{
              selectedRows: selectedInvoices,
              onRowSelect: (index: number) => {
                const newSelected = new Set(selectedInvoices)
                const invoice = filteredInvoices[index]
                if (newSelected.has(invoice.invoice_key)) {
                  newSelected.delete(invoice.invoice_key)
                } else {
                  newSelected.add(invoice.invoice_key)
                }
                setSelectedInvoices(newSelected)
              },
              onSelectAll: () => {
                if (selectedInvoices.size === filteredInvoices.length) {
                  setSelectedInvoices(new Set())
                } else {
                  setSelectedInvoices(new Set(filteredInvoices.map(inv => inv.invoice_key)))
                }
              }
            }}
          />
        </Card>
      </main>

      {/* New Invoice Modal */}
      <Modal
        isOpen={showNewInvoiceModal}
        onClose={() => setShowNewInvoiceModal(false)}
        title="Create New Invoice"
      >
        <div className="p-6">
          <p className="text-gray-600 mb-4">
            Invoice creation form will be implemented here.
          </p>
          <div className="flex justify-end space-x-3">
            <Button variant="outline" onClick={() => setShowNewInvoiceModal(false)}>
              Cancel
            </Button>
            <Button onClick={() => {
              alert('Creating invoice...')
              setShowNewInvoiceModal(false)
            }}>
              Create Invoice
            </Button>
          </div>
        </div>
      </Modal>
    </div>
  )
}