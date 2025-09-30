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
  XCircleIcon,
  ArrowDownTrayIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import Input from '@/components/UI/Input'
import Table from '@/components/UI/Table'
import PageHeader from '@/components/Layout/PageHeader'
import Modal from '@/components/UI/Modal'
import Select from '@/components/UI/Select'
import { formatCurrency, formatDate } from '@/lib/utils'
import { ReportGenerator } from '@/lib/reportGenerator'
import { EmailService } from '@/lib/emailService'

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
  payment_terms: string
  lines: {
    item_code: string
    description: string
    quantity: number
    unit_price: number
    amount: number
    tax_code: string
    tax_amount: number
  }[]
}

interface Customer {
  customer_code: string
  customer_name: string
  payment_terms: string
  credit_limit: number
  balance: number
}

export default function InvoicesPage() {
  const router = useRouter()
  const [invoices, setInvoices] = useState<Invoice[]>([])
  const [loading, setLoading] = useState(true)
  const [searchTerm, setSearchTerm] = useState('')
  const [statusFilter, setStatusFilter] = useState('all')
  const [dateRange, setDateRange] = useState({ from: '', to: '' })
  const [amountRange, setAmountRange] = useState({ min: '', max: '' })
  const [customerFilter, setCustomerFilter] = useState('')
  const [overdueFilter, setOverdueFilter] = useState('all')
  const [showNewInvoiceModal, setShowNewInvoiceModal] = useState(false)
  const [showEditInvoiceModal, setShowEditInvoiceModal] = useState(false)
  const [selectedInvoice, setSelectedInvoice] = useState<Invoice | null>(null)
  const [selectedInvoices, setSelectedInvoices] = useState<Set<number>>(new Set())
  const [customers, setCustomers] = useState<Customer[]>([])
  const [invoiceForm, setInvoiceForm] = useState<InvoiceFormData>({
    customer_code: '',
    invoice_date: new Date().toISOString().split('T')[0],
    due_date: '',
    reference: '',
    payment_terms: '30',
    lines: [{
      item_code: '',
      description: '',
      quantity: 1,
      unit_price: 0,
      amount: 0,
      tax_code: 'STD',
      tax_amount: 0
    }]
  })

  useEffect(() => {
    fetchInvoices()
    fetchCustomers()
  }, [])

  const fetchCustomers = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/customers/cobol/active')
      if (response.ok) {
        const data = await response.json()
        setCustomers(data.customers || [])
      }
    } catch (error) {
      console.error('Failed to fetch customers:', error)
    }
  }

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

  // Filter invoices based on search and status
  const filteredInvoices = invoices.filter(invoice => {
    // Search filter
    const matchesSearch = 
      (invoice.invoice_number || '').toString().toLowerCase().includes(searchTerm.toLowerCase()) ||
      (invoice.customer_name || '').toString().toLowerCase().includes(searchTerm.toLowerCase()) ||
      (invoice.invoice_reference || '').toString().toLowerCase().includes(searchTerm.toLowerCase())
    
    // Status filter
    let matchesStatus = true
    if (statusFilter !== 'all') {
      matchesStatus = invoice.invoice_status === statusFilter
    }
    
    // Date range filter
    let matchesDate = true
    if (dateRange.from || dateRange.to) {
      const invoiceDate = new Date(invoice.invoice_date)
      if (dateRange.from && invoiceDate < new Date(dateRange.from)) matchesDate = false
      if (dateRange.to && invoiceDate > new Date(dateRange.to)) matchesDate = false
    }
    
    // Amount range filter
    let matchesAmount = true
    if (amountRange.min || amountRange.max) {
      const amount = invoice.invoice_total_amount
      if (amountRange.min && amount < parseFloat(amountRange.min)) matchesAmount = false
      if (amountRange.max && amount > parseFloat(amountRange.max)) matchesAmount = false
    }
    
    // Customer filter
    let matchesCustomer = true
    if (customerFilter) {
      matchesCustomer = invoice.invoice_customer === customerFilter
    }
    
    // Overdue filter
    let matchesOverdue = true
    if (overdueFilter !== 'all') {
      const today = new Date()
      const dueDate = new Date(invoice.invoice_due_date)
      const daysDiff = Math.floor((today.getTime() - dueDate.getTime()) / (1000 * 60 * 60 * 24))
      
      switch (overdueFilter) {
        case 'current':
          matchesOverdue = invoice.invoice_status === 'O' && daysDiff <= 0
          break
        case 'overdue':
          matchesOverdue = invoice.invoice_status === 'O' && daysDiff > 0
          break
        case 'overdue30':
          matchesOverdue = invoice.invoice_status === 'O' && daysDiff > 30
          break
        case 'overdue60':
          matchesOverdue = invoice.invoice_status === 'O' && daysDiff > 60
          break
        case 'overdue90':
          matchesOverdue = invoice.invoice_status === 'O' && daysDiff > 90
          break
      }
    }
    
    return matchesSearch && matchesStatus && matchesDate && matchesAmount && matchesCustomer && matchesOverdue
  })

  const handlePrintInvoices = async () => {
    const selected = Array.from(selectedInvoices)
    if (selected.length === 0) {
      alert('Please select invoices to print')
      return
    }
    
    // Generate a summary report of selected invoices
    const selectedInvoiceData = filteredInvoices.filter(inv => selected.includes(inv.invoice_key))
    
    const reportData = {
      title: 'Selected Invoices',
      subtitle: `${selected.length} invoice(s) selected for printing`,
      generatedDate: new Date(),
      headers: ['Invoice #', 'Customer', 'Date', 'Due Date', 'Amount', 'Balance', 'Status'],
      rows: selectedInvoiceData.map(inv => [
        inv.invoice_number,
        inv.customer_name,
        formatDate(inv.invoice_date),
        formatDate(inv.invoice_due_date),
        formatCurrency(inv.invoice_total_amount),
        formatCurrency(inv.invoice_balance),
        inv.invoice_status === 'O' ? 'Open' : 
        inv.invoice_status === 'P' ? 'Paid' : 
        inv.invoice_status === 'D' ? 'Draft' : 'Cancelled'
      ]),
      summary: [
        { label: 'Total Invoices', value: selected.length.toString() },
        { label: 'Total Amount', value: formatCurrency(selectedInvoiceData.reduce((sum, inv) => sum + inv.invoice_total_amount, 0)) },
        { label: 'Total Outstanding', value: formatCurrency(selectedInvoiceData.reduce((sum, inv) => sum + inv.invoice_balance, 0)) }
      ]
    }
    
    await ReportGenerator.generatePDF(reportData)
  }

  const handleEmailInvoices = () => {
    const selected = Array.from(selectedInvoices)
    if (selected.length === 0) {
      alert('Please select invoices to email')
      return
    }
    
    // Get selected invoice data
    const selectedInvoiceData = filteredInvoices.filter(inv => selected.includes(inv.invoice_key))
    
    if (selected.length === 1) {
      // Single invoice - email with details
      const invoice = selectedInvoiceData[0]
      const customerEmail = prompt(`Enter email address for ${invoice.customer_name}:`, 'customer@example.com')
      
      if (customerEmail) {
        EmailService.sendInvoice(invoice, customerEmail)
      }
    } else {
      // Multiple invoices - ask for email and send summary
      const customerEmail = prompt('Enter recipient email address:', 'customer@example.com')
      
      if (customerEmail) {
        EmailService.sendMultipleInvoices(selectedInvoiceData, customerEmail)
      }
    }
  }

  const handleDuplicateInvoice = (invoice: Invoice) => {
    alert(`Creating copy of invoice ${invoice.invoice_number}...`)
  }

  const handleViewInvoice = (invoice: Invoice) => {
    router.push(`/sales/invoices/${invoice.invoice_key}`)
  }

  const handleEditInvoice = (invoice: Invoice) => {
    if (invoice.invoice_status !== 'D') {
      alert('Only draft invoices can be edited')
      return
    }
    setSelectedInvoice(invoice)
    // TODO: Fetch full invoice details and populate form
    setShowEditInvoiceModal(true)
  }

  const handleDeleteInvoice = async (invoice: Invoice) => {
    if (invoice.invoice_status !== 'D') {
      alert('Only draft invoices can be deleted')
      return
    }
    
    if (!confirm(`Are you sure you want to delete invoice ${invoice.invoice_number}?`)) {
      return
    }

    try {
      const response = await fetch(`http://localhost:8000/api/v1/sl/invoices/${invoice.invoice_key}`, {
        method: 'DELETE',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer demo-token'
        }
      })

      if (response.ok) {
        alert('Invoice deleted successfully')
        fetchInvoices()
      } else {
        const error = await response.json()
        alert(`Failed to delete invoice: ${error.detail || 'Unknown error'}`)
      }
    } catch (error) {
      alert('Failed to delete invoice')
      console.error(error)
    }
  }

  const handleCreateInvoice = async () => {
    // Validate form
    if (!invoiceForm.customer_code) {
      alert('Please select a customer')
      return
    }

    if (invoiceForm.lines.length === 0 || invoiceForm.lines.every(line => !line.description)) {
      alert('Please add at least one line item')
      return
    }

    try {
      const response = await fetch('http://localhost:8000/api/v1/sl/invoices', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer demo-token'
        },
        body: JSON.stringify({
          customer_code: invoiceForm.customer_code,
          invoice_date: invoiceForm.invoice_date,
          due_date: invoiceForm.due_date,
          reference: invoiceForm.reference,
          lines: invoiceForm.lines.filter(line => line.description).map(line => ({
            ...line,
            amount: line.quantity * line.unit_price
          }))
        })
      })

      if (response.ok) {
        const newInvoice = await response.json()
        alert(`Invoice ${newInvoice.invoice_number} created successfully`)
        setShowNewInvoiceModal(false)
        resetInvoiceForm()
        fetchInvoices()
      } else {
        const error = await response.json()
        alert(`Failed to create invoice: ${error.detail || 'Unknown error'}`)
      }
    } catch (error) {
      alert('Failed to create invoice')
      console.error(error)
    }
  }

  const resetInvoiceForm = () => {
    setInvoiceForm({
      customer_code: '',
      invoice_date: new Date().toISOString().split('T')[0],
      due_date: '',
      reference: '',
      payment_terms: '30',
      lines: [{
        item_code: '',
        description: '',
        quantity: 1,
        unit_price: 0,
        amount: 0,
        tax_code: 'STD',
        tax_amount: 0
      }]
    })
  }

  const addInvoiceLine = () => {
    setInvoiceForm({
      ...invoiceForm,
      lines: [...invoiceForm.lines, {
        item_code: '',
        description: '',
        quantity: 1,
        unit_price: 0,
        amount: 0,
        tax_code: 'STD',
        tax_amount: 0
      }]
    })
  }

  const removeInvoiceLine = (index: number) => {
    setInvoiceForm({
      ...invoiceForm,
      lines: invoiceForm.lines.filter((_, i) => i !== index)
    })
  }

  const updateInvoiceLine = (index: number, field: string, value: any) => {
    const newLines = [...invoiceForm.lines]
    newLines[index] = { ...newLines[index], [field]: value }
    
    // Calculate amount if quantity or unit price changed
    if (field === 'quantity' || field === 'unit_price') {
      newLines[index].amount = newLines[index].quantity * newLines[index].unit_price
      // Simple tax calculation (assuming 20% VAT for STD tax code)
      if (newLines[index].tax_code === 'STD') {
        newLines[index].tax_amount = newLines[index].amount * 0.20
      }
    }
    
    setInvoiceForm({ ...invoiceForm, lines: newLines })
  }

  const calculateDueDate = (paymentTerms: string) => {
    const invoiceDate = new Date(invoiceForm.invoice_date)
    const daysToAdd = parseInt(paymentTerms) || 30
    invoiceDate.setDate(invoiceDate.getDate() + daysToAdd)
    return invoiceDate.toISOString().split('T')[0]
  }

  const getStatusLabel = (status: string): string => {
    switch (status) {
      case 'D': return 'Draft'
      case 'O': return 'Open'
      case 'P': return 'Paid'
      case 'C': return 'Cancelled'
      default: return 'Unknown'
    }
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
            onClick={() => handleDeleteInvoice(row)}
            className="text-red-500 hover:text-red-700"
            title="Delete invoice"
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
                value={statusFilter}
                onChange={(e) => setStatusFilter(e.target.value)}
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

        {/* Bulk Actions Bar */}
        {selectedInvoices.size > 0 && (
          <div className="mb-4 bg-gray-50 px-6 py-3 rounded-lg shadow-sm">
            <div className="flex items-center justify-between">
              <div className="flex items-center space-x-2">
                <span className="text-sm font-medium text-gray-700">
                  {selectedInvoices.size} invoice{selectedInvoices.size !== 1 ? 's' : ''} selected
                </span>
                <Button
                  size="sm"
                  variant="outline"
                  onClick={() => setSelectedInvoices(new Set())}
                >
                  Clear Selection
                </Button>
              </div>
              <div className="flex items-center space-x-2">
                <Button
                  size="sm"
                  variant="outline"
                  onClick={async () => {
                    const selectedInvoiceData = filteredInvoices.filter(inv => 
                      selectedInvoices.has(inv.invoice_key)
                    )
                    
                    // Generate combined PDF
                    const invoicesData = {
                      invoices: selectedInvoiceData,
                      title: 'Batch Invoice Export',
                      generatedDate: new Date()
                    }
                    
                    await ReportGenerator.exportBatchInvoices(invoicesData, 'pdf')
                  }}
                >
                  <PrinterIcon className="h-4 w-4" />
                  Print Selected
                </Button>
                <Button
                  size="sm"
                  variant="outline"
                  onClick={() => {
                    const selectedInvoiceData = filteredInvoices.filter(inv => 
                      selectedInvoices.has(inv.invoice_key)
                    )
                    
                    // Create CSV export
                    const csvData = selectedInvoiceData.map(inv => ({
                      'Invoice Number': inv.invoice_number,
                      'Customer': inv.customer_name,
                      'Date': formatDate(inv.invoice_date),
                      'Due Date': formatDate(inv.invoice_due_date),
                      'Total': inv.invoice_total_amount,
                      'Paid': inv.invoice_paid_amount,
                      'Balance': inv.invoice_balance,
                      'Status': getStatusLabel(inv.invoice_status)
                    }))
                    
                    ReportGenerator.exportToCSV(csvData, 'invoices_export')
                  }}
                >
                  <ArrowDownTrayIcon className="h-4 w-4" />
                  Export CSV
                </Button>
                <Button
                  size="sm"
                  variant="outline"
                  onClick={async () => {
                    const selectedInvoiceData = filteredInvoices.filter(inv => 
                      selectedInvoices.has(inv.invoice_key) && inv.invoice_status === 'O'
                    )
                    
                    if (selectedInvoiceData.length === 0) {
                      alert('No open invoices selected')
                      return
                    }
                    
                    const recipientEmail = prompt('Enter recipient email address:', 'customer@example.com')
                    if (recipientEmail) {
                      EmailService.sendBatchInvoices(selectedInvoiceData, recipientEmail)
                    }
                  }}
                >
                  <EnvelopeIcon className="h-4 w-4" />
                  Email Selected
                </Button>
                <Button
                  size="sm"
                  className="bg-green-600 hover:bg-green-700"
                  onClick={async () => {
                    const openInvoices = filteredInvoices.filter(inv => 
                      selectedInvoices.has(inv.invoice_key) && inv.invoice_status === 'O'
                    )
                    
                    if (openInvoices.length === 0) {
                      alert('No open invoices selected')
                      return
                    }
                    
                    if (!confirm(`Mark ${openInvoices.length} invoice(s) as paid?`)) {
                      return
                    }
                    
                    try {
                      // Process each invoice
                      for (const invoice of openInvoices) {
                        await fetch(`http://localhost:8000/api/v1/sales/invoices/${invoice.invoice_key}/mark-paid`, {
                          method: 'POST',
                          headers: { 'Content-Type': 'application/json' }
                        })
                      }
                      
                      alert(`${openInvoices.length} invoice(s) marked as paid`)
                      setSelectedInvoices(new Set())
                      fetchInvoices() // Refresh
                    } catch (error) {
                      console.error('Error marking invoices as paid:', error)
                      alert('Failed to mark some invoices as paid')
                    }
                  }}
                >
                  <CheckCircleIcon className="h-4 w-4" />
                  Mark as Paid
                </Button>
                <Button
                  size="sm"
                  variant="danger"
                  onClick={async () => {
                    const deletableInvoices = filteredInvoices.filter(inv => 
                      selectedInvoices.has(inv.invoice_key) && inv.invoice_status === 'D'
                    )
                    
                    if (deletableInvoices.length === 0) {
                      alert('Only draft invoices can be deleted')
                      return
                    }
                    
                    if (!confirm(`Delete ${deletableInvoices.length} draft invoice(s)? This cannot be undone.`)) {
                      return
                    }
                    
                    try {
                      // Delete each invoice
                      for (const invoice of deletableInvoices) {
                        await fetch(`http://localhost:8000/api/v1/sales/invoices/${invoice.invoice_key}`, {
                          method: 'DELETE'
                        })
                      }
                      
                      alert(`${deletableInvoices.length} invoice(s) deleted`)
                      setSelectedInvoices(new Set())
                      fetchInvoices() // Refresh
                    } catch (error) {
                      console.error('Error deleting invoices:', error)
                      alert('Failed to delete some invoices')
                    }
                  }}
                >
                  <TrashIcon className="h-4 w-4" />
                  Delete Selected
                </Button>
              </div>
            </div>
          </div>
        )}

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
        onClose={() => {
          setShowNewInvoiceModal(false)
          resetInvoiceForm()
        }}
        title="Create New Invoice"
        className="max-w-4xl"
      >
        <div className="p-6 space-y-6">
          {/* Customer and Basic Info */}
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-1">
                Customer
              </label>
              <Select
                value={invoiceForm.customer_code}
                onChange={(e) => {
                  const customer = customers.find(c => c.customer_code === e.target.value)
                  setInvoiceForm({
                    ...invoiceForm,
                    customer_code: e.target.value,
                    payment_terms: customer?.payment_terms || '30',
                    due_date: calculateDueDate(customer?.payment_terms || '30')
                  })
                }}
                options={[
                  { value: '', label: 'Select a customer' },
                  ...customers.map(c => ({
                    value: c.customer_code,
                    label: `${c.customer_code} - ${c.customer_name}`
                  }))
                ]}
                required
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-1">
                Reference
              </label>
              <Input
                type="text"
                value={invoiceForm.reference}
                onChange={(e) => setInvoiceForm({ ...invoiceForm, reference: e.target.value })}
                placeholder="PO number or reference"
              />
            </div>
          </div>

          <div className="grid grid-cols-3 gap-4">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-1">
                Invoice Date
              </label>
              <Input
                type="date"
                value={invoiceForm.invoice_date}
                onChange={(e) => setInvoiceForm({ ...invoiceForm, invoice_date: e.target.value })}
                required
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-1">
                Payment Terms (days)
              </label>
              <Input
                type="number"
                value={invoiceForm.payment_terms}
                onChange={(e) => {
                  setInvoiceForm({
                    ...invoiceForm,
                    payment_terms: e.target.value,
                    due_date: calculateDueDate(e.target.value)
                  })
                }}
                min="0"
                required
              />
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-1">
                Due Date
              </label>
              <Input
                type="date"
                value={invoiceForm.due_date}
                onChange={(e) => setInvoiceForm({ ...invoiceForm, due_date: e.target.value })}
                required
              />
            </div>
          </div>

          {/* Line Items */}
          <div>
            <div className="flex justify-between items-center mb-2">
              <label className="block text-sm font-medium text-gray-700">
                Line Items
              </label>
              <Button
                size="sm"
                variant="outline"
                onClick={addInvoiceLine}
              >
                <PlusIcon className="h-4 w-4" />
                Add Line
              </Button>
            </div>
            
            <div className="border rounded-lg overflow-hidden">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-3 py-2 text-left text-xs font-medium text-gray-500">Description</th>
                    <th className="px-3 py-2 text-left text-xs font-medium text-gray-500">Qty</th>
                    <th className="px-3 py-2 text-left text-xs font-medium text-gray-500">Unit Price</th>
                    <th className="px-3 py-2 text-left text-xs font-medium text-gray-500">Tax</th>
                    <th className="px-3 py-2 text-right text-xs font-medium text-gray-500">Amount</th>
                    <th className="px-3 py-2"></th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {invoiceForm.lines.map((line, index) => (
                    <tr key={index}>
                      <td className="px-3 py-2">
                        <Input
                          type="text"
                          value={line.description}
                          onChange={(e) => updateInvoiceLine(index, 'description', e.target.value)}
                          placeholder="Item description"
                          className="w-full"
                          size="sm"
                        />
                      </td>
                      <td className="px-3 py-2">
                        <Input
                          type="number"
                          value={line.quantity}
                          onChange={(e) => updateInvoiceLine(index, 'quantity', parseFloat(e.target.value) || 0)}
                          min="0"
                          step="0.01"
                          className="w-20"
                          size="sm"
                        />
                      </td>
                      <td className="px-3 py-2">
                        <Input
                          type="number"
                          value={line.unit_price}
                          onChange={(e) => updateInvoiceLine(index, 'unit_price', parseFloat(e.target.value) || 0)}
                          min="0"
                          step="0.01"
                          className="w-24"
                          size="sm"
                        />
                      </td>
                      <td className="px-3 py-2">
                        <Select
                          value={line.tax_code}
                          onChange={(e) => updateInvoiceLine(index, 'tax_code', e.target.value)}
                          options={[
                            { value: 'STD', label: 'STD (20%)' },
                            { value: 'ZERO', label: 'Zero (0%)' },
                            { value: 'EXEMPT', label: 'Exempt' }
                          ]}
                          size="sm"
                        />
                      </td>
                      <td className="px-3 py-2 text-right">
                        {formatCurrency(line.amount)}
                      </td>
                      <td className="px-3 py-2">
                        <button
                          onClick={() => removeInvoiceLine(index)}
                          className="text-red-500 hover:text-red-700"
                          disabled={invoiceForm.lines.length === 1}
                        >
                          <TrashIcon className="h-4 w-4" />
                        </button>
                      </td>
                    </tr>
                  ))}
                </tbody>
                <tfoot className="bg-gray-50">
                  <tr>
                    <td colSpan={4} className="px-3 py-2 text-right font-medium">
                      Subtotal:
                    </td>
                    <td className="px-3 py-2 text-right font-medium">
                      {formatCurrency(invoiceForm.lines.reduce((sum, line) => sum + line.amount, 0))}
                    </td>
                    <td></td>
                  </tr>
                  <tr>
                    <td colSpan={4} className="px-3 py-2 text-right font-medium">
                      Tax:
                    </td>
                    <td className="px-3 py-2 text-right font-medium">
                      {formatCurrency(invoiceForm.lines.reduce((sum, line) => sum + line.tax_amount, 0))}
                    </td>
                    <td></td>
                  </tr>
                  <tr>
                    <td colSpan={4} className="px-3 py-2 text-right font-bold">
                      Total:
                    </td>
                    <td className="px-3 py-2 text-right font-bold">
                      {formatCurrency(invoiceForm.lines.reduce((sum, line) => sum + line.amount + line.tax_amount, 0))}
                    </td>
                    <td></td>
                  </tr>
                </tfoot>
              </table>
            </div>
          </div>

          {/* Actions */}
          <div className="flex justify-end space-x-3">
            <Button 
              variant="outline" 
              onClick={() => {
                setShowNewInvoiceModal(false)
                resetInvoiceForm()
              }}
            >
              Cancel
            </Button>
            <Button onClick={handleCreateInvoice}>
              Create Invoice
            </Button>
          </div>
        </div>
      </Modal>
    </div>
  )
}